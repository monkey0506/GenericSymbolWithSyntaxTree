using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Operations;
using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

namespace Monkeymoto.GeneratorUtils
{
    /// <summary>
    /// Represents a generic <see cref="ISymbol"/> and an associated <see cref="SyntaxNode"/>.
    /// </summary>
    public sealed class GenericSymbolReference : IEquatable<GenericSymbolReference>
    {
        private readonly int hashCode;

        /// <summary>
        /// Represents whether <see cref="Symbol">Symbol</see> is a closed generic type or closed generic method.
        /// </summary>
        /// <remarks>
        /// <para>
        /// This property only represents whether the <see cref="Symbol">Symbol</see> itself is a closed generic type
        /// or closed generic method. This does not check the containing type or method. To check if the
        /// <see cref="Node">Node</see> represents a source location with no open generic type parameters, see
        /// <see cref="IsSyntaxReferenceClosedTypeOrMethod">IsSyntaxReferenceClosedTypeOrMethod</see> instead.
        /// </para>
        /// </remarks>
        /// <returns>
        /// <see langword="true"/> if <see cref="Symbol">Symbol</see> has no unsubstituted type arguments; otherwise,
        /// <see langword="false"/>.
        /// </returns>
        public bool IsClosedTypeOrMethod { get; }
        /// <summary>
        /// Represents whether the <see cref="Node">Node</see> represents a source location with no open generic type
        /// parameters.
        /// </summary>
        /// <remarks>
        /// <para>
        /// This property will check containing types and methods. If this <see cref="Node">Node</see> is inside of an
        /// open generic type or method (at any level of nesting), then this property will return
        /// <see langword="false"/>, regardless of whether <see cref="Symbol">Symbol</see> is a closed type or method.
        /// To only check <see cref="Symbol">Symbol</see> see
        /// <see cref="IsClosedTypeOrMethod">IsClosedTypeOrMethod</see> instead.
        /// </para>
        /// </remarks>
        /// <returns>
        /// <see langword="true"/> if <see cref="Node">Node</see> references a source location with no unsubstituted
        /// type arguments; otherwise, <see langword="false"/>.
        /// </returns>
        public bool IsSyntaxReferenceClosedTypeOrMethod { get; }
        /// <summary>
        /// Represents the <see cref="SyntaxNode"/> associated with <see cref="Symbol">Symbol</see>.
        /// </summary>
        public SyntaxNode Node { get; }
        /// <summary>
        /// Represents the <see cref="SemanticModel"/> used to acquire <see cref="Symbol">Symbol</see>.
        /// </summary>
        public SemanticModel SemanticModel { get; }
        /// <summary>
        /// Represents the <see cref="ISymbol"/> associated with <see cref="Node">Node</see>.
        /// </summary>
        public ISymbol Symbol { get; }
        /// <summary>
        /// Represents the type arguments for this <see cref="Symbol">Symbol</see>.
        /// </summary>
        /// <remarks>
        /// <para>
        /// If <see cref="IsClosedTypeOrMethod">IsClosedTypeOrMethod</see> is <see langword="false"/>, then one or more
        /// of these type arguments is an <see cref="ITypeParameterSymbol"/> or an open generic type; otherwise, these
        /// values represent the closed type arguments.
        /// </para>
        /// </remarks>
        public ImmutableArray<ITypeSymbol> TypeArguments { get; }

        public static bool operator ==(GenericSymbolReference? left, GenericSymbolReference? right) =>
            left?.Equals(right) ?? right is null;
        public static bool operator !=(GenericSymbolReference? left, GenericSymbolReference? right) =>
            !(left == right);

        internal static GenericSymbolReference? FromSyntaxNodeInternal
        (
            SyntaxNode node,
            SemanticModel semanticModel,
            CancellationToken cancellationToken
        )
        {
            ISymbol? symbol;
            ImmutableArray<ITypeSymbol> typeArguments;
            if (node is IdentifierNameSyntax)
            {
                node = node.Parent switch
                {
                    InvocationExpressionSyntax => node.Parent,
                    MemberAccessExpressionSyntax => node.Parent.Parent!,
                    _ => node
                };
                IMethodSymbol? methodSymbol = semanticModel.GetOperation(node, cancellationToken) switch
                {
                    IInvocationOperation { TargetMethod.IsGenericMethod: true } invocation =>
                        invocation.TargetMethod,
                    IMethodReferenceOperation { Method.IsGenericMethod: true } methodReference =>
                        methodReference.Method,
                    _ => null
                };
                if (methodSymbol is null)
                {
                    return null;
                }
                symbol = methodSymbol;
                typeArguments = methodSymbol.TypeArguments;
            }
            else
            {
                symbol = semanticModel.GetSymbolInfo(node, cancellationToken).Symbol;
                switch (symbol)
                {
                    case null or ISymbol { IsDefinition: true }:
                        return null;
                    case IMethodSymbol { IsGenericMethod: true } methodSymbol:
                        typeArguments = methodSymbol.TypeArguments;
                        break;
                    case INamedTypeSymbol { IsGenericType: true } namedTypeSymbol:
                        typeArguments = namedTypeSymbol.TypeArguments;
                        break;
                    default:
                        return null;
                }
            }
            return new GenericSymbolReference(node, semanticModel, symbol, typeArguments, cancellationToken);
        }

        /// <summary>
        /// Gets the hash code for this <see cref="GenericSymbolReference"/>.
        /// </summary>
        /// <remarks>
        /// <para>
        /// Borrowed from <see href="https://stackoverflow.com/a/1646913">Quick and Simple Hash Code Combinations -
        /// Stack Overflow</see> answer by user <see href="https://stackoverflow.com/users/22656/jon-skeet">Jon
        /// Skeet</see>, licensed under <see href="https://creativecommons.org/licenses/by-sa/2.5/">CC BY-SA 2.5</see>.
        /// Changes have been made to match the fields of this class.
        /// </para>
        /// </remarks>
        /// <returns>The hash value generated for this <see cref="GenericSymbolReference"/>.</returns>
        private static int GetHashCode(SyntaxNode node, ISymbol symbol)
        {
            unchecked
            {
                int hash = 17;
                hash = hash * 31 + node.GetHashCode();
                hash = hash * 31 + SymbolEqualityComparer.Default.GetHashCode(symbol);
                return hash;
            }
        }

        internal static bool IsOpenTypeOrMethodSymbol(ISymbol symbol)
        {
            return symbol switch
            {
                ITypeParameterSymbol => true,
                IMethodSymbol methodSymbol => methodSymbol.TypeArguments.Any(IsOpenTypeOrMethodSymbol),
                INamedTypeSymbol namedTypeSymbol => namedTypeSymbol.TypeArguments.Any(IsOpenTypeOrMethodSymbol),
                _ => false
            };
        }

        private static bool IsSyntaxReferenceClosedTypeOrMethodSymbol(SyntaxNode node, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            var symbol = semanticModel.GetSymbolInfo(node, cancellationToken).Symbol;
            if (symbol is null)
            {
                return false;
            }
            while (symbol is not null)
            {
                if (IsOpenTypeOrMethodSymbol(symbol))
                {
                    return false;
                }
                symbol = symbol.ContainingSymbol;
            }
            return true;
        }

        internal GenericSymbolReference
        (
            SyntaxNode node,
            SemanticModel semanticModel,
            ISymbol symbol,
            ImmutableArray<ITypeSymbol> typeArguments,
            CancellationToken cancellationToken
        )
        {
            IsClosedTypeOrMethod = !IsOpenTypeOrMethodSymbol(symbol);
            IsSyntaxReferenceClosedTypeOrMethod = IsSyntaxReferenceClosedTypeOrMethodSymbol(node, semanticModel, cancellationToken);
            Node = node;
            SemanticModel = semanticModel;
            Symbol = symbol;
            TypeArguments = typeArguments;
            hashCode = GetHashCode(node, symbol);
        }

        public override bool Equals(object? obj) => obj is GenericSymbolReference other && Equals(other);
        public bool Equals(GenericSymbolReference? other) => (other is not null) && Node.IsEquivalentTo(other.Node) &&
            SymbolEqualityComparer.Default.Equals(Symbol, other.Symbol);
        public override int GetHashCode() => hashCode;
    }
}
