using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Monkeymoto.GeneratorUtils
{
    /// <summary>
    /// Represents a collection of closed generic symbols for use with an incremental source generator.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class discovers generic types and generic methods in your compilation and keeps
    /// <see cref="ISymbol">symbolic references</see> to them and the <see cref="SyntaxNode">syntax</see> that produced
    /// those references. This may create pressure on your compilation in terms of memory or time spent discovering
    /// those symbols.
    /// </para><para>
    /// Open generic symbols are resolved to closed generic symbols by calling
    /// <see cref="GetBranchesBySymbol">GetBranchesBySymbol</see>.
    /// </para>
    /// </remarks>
    public sealed class GenericSymbolReferenceTree : IDisposable
    {
        private readonly Dictionary<ISymbol, HashSet<GenericSymbolReference>> closedBranches =
            new(SymbolEqualityComparer.Default);
        private readonly Dictionary<ISymbol, HashSet<GenericSymbolReference>> openBranches =
            new(SymbolEqualityComparer.Default);

        /// <summary>
        /// Creates a new tree from an incremental generator initialization context.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The returned tree should <b>not</b> be a long-living object. You should extract the symbol references you
        /// need from the tree and then call <see cref="Dispose">Dispose</see> to free the memory used by the tree.
        /// </para>
        /// </remarks>
        /// <param name="context">The context used to create the new tree.</param>
        /// <returns>An <see cref="IncrementalValueProvider{TValue}"/> which provides the newly created tree.</returns>
        public static IncrementalValueProvider<GenericSymbolReferenceTree>
            FromIncrementalGeneratorInitializationContext
        (
            IncrementalGeneratorInitializationContext context
        )
        {
            return FromIncrementalGeneratorInitializationContext(context, static x => false);
        }

        /// <inheritdoc cref="FromIncrementalGeneratorInitializationContext(IncrementalGeneratorInitializationContext)"/>
        /// <param name="excludePathPredicate">
        /// A predicate used to selectively exclude certain file paths from the tree. For example, you may choose to
        /// exclude file paths your generator added to the compilation that include only definitions. The predicate
        /// receives the full file path of each <see cref="SyntaxNode">SyntaxNode</see>'s
        /// <see cref="SyntaxTree">SyntaxTree</see> considered for inclusion in the
        /// <see cref="GenericSymbolReferenceTree">GenericSymbolReferenceTree</see>.
        /// </param>
        public static IncrementalValueProvider<GenericSymbolReferenceTree>
            FromIncrementalGeneratorInitializationContext
        (
            IncrementalGeneratorInitializationContext context,
            Func<string, bool> excludePathPredicate
        )
        {
            var symbolsProvider = context.SyntaxProvider.CreateSyntaxProvider
            (
                (node, cancellationToken) =>
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    return node switch
                    {
                        GenericNameSyntax => true,
                        IdentifierNameSyntax identifierName =>
                            identifierName.Parent is ArgumentSyntax or EqualsValueClauseSyntax,
                        _ => false
                    } && !excludePathPredicate(node.SyntaxTree.FilePath);
                },
                static (context, cancellationToken) =>
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    return GenericSymbolReference.FromSyntaxNodeInternal
                    (
                        context.Node,
                        context.SemanticModel,
                        cancellationToken
                    );
                }
            );
            return symbolsProvider.Collect()
                .Select
                (
                    static (references, cancellationToken) =>
                        new GenericSymbolReferenceTree(references, cancellationToken)
                );
        }

        private GenericSymbolReferenceTree
        (
            ImmutableArray<GenericSymbolReference?> references,
            CancellationToken cancellationToken
        )
        {
            foreach (var reference in references)
            {
                static void AddReference
                (
                    Dictionary<ISymbol, HashSet<GenericSymbolReference>> branches,
                    GenericSymbolReference reference
                )
                {
                    if (branches.TryGetValue(reference.Symbol, out var set))
                    {
                        _ = set.Add(reference);
                    }
                    else
                    {
                        branches[reference.Symbol] = [reference];
                    }
                }

                cancellationToken.ThrowIfCancellationRequested();
                switch (reference?.IsClosedTypeOrMethod)
                {
                    case true:
                        AddReference(closedBranches, reference);
                        break;
                    case false:
                        AddReference(openBranches, reference);
                        break;
                    case null:
                    default:
                        break;
                }
            }
        }

        /// <summary>
        /// Removes all references in the tree, releasing its memory.
        /// </summary>
        public void Dispose()
        {
            closedBranches.Clear();
            openBranches.Clear();
        }

        /// <summary>
        /// Returns a collection of all branches in the tree that match the given symbol.
        /// </summary>
        /// <remarks>
        /// <para>
        /// If <paramref name="symbol"/> is an open generic symbol, this method will discover all branches that match
        /// <paramref name="symbol"/> after type substitutions. If <paramref name="symbol"/> is the
        /// <see cref="ISymbol.OriginalDefinition">original symbol definition</see>, this method will discover all
        /// branches that share the same original symbol.
        /// </para><para>
        /// If <paramref name="symbol"/> is a closed generic symbol, then the returned collection will only represent
        /// those syntax nodes which reference this closed symbol.
        /// </para>
        /// </remarks>
        /// <param name="symbol">The generic symbol to find in the tree.</param>
        /// <param name="cancellationToken">
        /// The <see cref="CancellationToken"/> that will be observed while searching the tree.
        /// </param>
        /// <returns>
        /// A flattened collection of all branches in the tree that match <paramref name="symbol"/>, regardless of the
        /// syntax node. The returned collection will only contain closed generic symbols.
        /// </returns>
        public IEnumerable<GenericSymbolReference> GetBranchesBySymbol
        (
            ISymbol symbol,
            CancellationToken cancellationToken
        )
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            IEnumerable<KeyValuePair<ISymbol, HashSet<GenericSymbolReference>>> GetBranches
            (
                Dictionary<ISymbol, HashSet<GenericSymbolReference>> branches
            )
            {
                return symbol.IsDefinition ?
                    branches.Where(x => SymbolEquals(x.Key.OriginalDefinition, symbol, cancellationToken)) :
                    branches.TryGetValue(symbol, out var set) ?
                        [new KeyValuePair<ISymbol, HashSet<GenericSymbolReference>>(symbol, set)] :
                        [];
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            static bool SymbolEquals(ISymbol? left, ISymbol? right, CancellationToken cancellationToken)
            {
                cancellationToken.ThrowIfCancellationRequested();
                return SymbolEqualityComparer.Default.Equals(left, right);
            }

            int typeArgumentCount;
            switch (symbol)
            {
                case null:
                case IMethodSymbol { IsGenericMethod: false }:
                case INamedTypeSymbol { IsGenericType: false }:
                    return [];
                case IMethodSymbol methodSymbol:
                    typeArgumentCount = methodSymbol.TypeArguments.Length;
                    break;
                case INamedTypeSymbol namedTypeSymbol:
                    typeArgumentCount = namedTypeSymbol.TypeArguments.Length;
                    break;
                default:
                    return [];
            }
            var searchBranches = GetBranches(openBranches);
            var closedBranchesForSymbol = GetBranches(closedBranches);
            if (!searchBranches.Any())
            {
                return closedBranchesForSymbol.SelectMany(static x => x.Value).Distinct();
            }
            searchBranches = searchBranches.ToImmutableArray();
            var typeArgumentSets = new List<HashSet<INamedTypeSymbol>>(typeArgumentCount);
            for (int i = 0; i < typeArgumentCount; ++i)
            {
                typeArgumentSets.Add(new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default));
            }
            foreach (var kv in searchBranches)
            {
                _ = openBranches.Remove(kv.Key);
                foreach (var reference in kv.Value)
                {
                    var typeArguments = reference.TypeArguments;
                    for (int i = 0; i < typeArgumentCount; ++i)
                    {
                        cancellationToken.ThrowIfCancellationRequested();
                        var typeArgument = typeArguments[i];
                        var typeArgumentSet = typeArgumentSets[i];
                        if (GenericSymbolReference.IsOpenTypeOrMethodSymbol(typeArgument))
                        {
                            typeArgumentSet.UnionWith
                            (
                                typeArgument switch
                                {
                                    ITypeParameterSymbol typeParameter =>
                                        GetBranchesBySymbol(typeParameter.ContainingSymbol, cancellationToken)
                                            .Select(x => (INamedTypeSymbol)x.TypeArguments[typeParameter.Ordinal]),
                                    _ => GetBranchesBySymbol(typeArgument, cancellationToken)
                                        .Select(static x => (INamedTypeSymbol)x.Symbol)
                                }
                            );
                        }
                        else
                        {
                            _ = typeArgumentSet.Add((INamedTypeSymbol)typeArgument);
                        }
                    }
                }
            }
            foreach (var closedReference in closedBranchesForSymbol.SelectMany(static x => x.Value))
            {
                cancellationToken.ThrowIfCancellationRequested();
                var typeArguments = closedReference.TypeArguments;
                for (int i = 0; i < typeArgumentCount; ++i)
                {
                    var typeArgument = typeArguments[i];
                    var typeArgumentSet = typeArgumentSets[i];
                    _ = typeArgumentSet.Add((INamedTypeSymbol)typeArgument);
                }
            }
            Func<ITypeSymbol[], ISymbol>? construct = symbol switch
            {
                IMethodSymbol methodSymbol => methodSymbol.OriginalDefinition.Construct,
                INamedTypeSymbol namedTypeSymbol => namedTypeSymbol.OriginalDefinition.Construct,
                _ => throw new UnreachableException(),
            };
            var constructedSymbols = new List<ISymbol>();
            foreach (var typeArgumentList in typeArgumentSets.CartesianProduct())
            {
                cancellationToken.ThrowIfCancellationRequested();
                constructedSymbols.Add(construct([.. typeArgumentList]));
            }
            if (!closedBranches.TryGetValue(symbol, out var set))
            {
                set = [];
                closedBranches[symbol] = set;
            }
            foreach (var reference in searchBranches.SelectMany(static x => x.Value))
            {
                cancellationToken.ThrowIfCancellationRequested();
                foreach (var constructedSymbol in constructedSymbols)
                {
                    var typeArguments = constructedSymbol switch
                    {
                        IMethodSymbol methodSymbol => methodSymbol.TypeArguments,
                        INamedTypeSymbol namedTypeSymbol => namedTypeSymbol.TypeArguments,
                        _ => []
                    };
                    _ = set.Add
                    (
                        new GenericSymbolReference
                        (
                            reference.Node,
                            reference.SemanticModel,
                            constructedSymbol,
                            typeArguments,
                            cancellationToken
                        )
                    );
                }
            }
            return GetBranches(closedBranches).SelectMany(static x => x.Value).Distinct();
        }
    }
}
