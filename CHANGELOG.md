# CHANGELOG.md

## 3.0.0 (2024-10-25)

 - Bugfix for
   [#3](https://github.com/monkey0506/Monkeymoto.GeneratorUtils.GenericSymbolReferenceTree/issues/3)
   (forced version update). This includes many breaking changes in the public
   API. Notably, `GenericSymbolReferenceTree.GetBranch` and the
   `GenericSymbolReference` public constructors have been removed.
 - Fix for
   [#2](https://github.com/monkey0506/Monkeymoto.GeneratorUtils.GenericSymbolReferenceTree/issues/2).
   This clarifies the meaning and improves the function of
   `GenericSymbolReference.IsSyntaxReferenceClosedTypeOrMethod`.

## 2.0.1 (2024-10-18)

 - Allow tracking the `SemanticModel` used to acquire symbols.
 - Make tree `IDisposable` to allow freeing memory.
 - Update `Microsoft.CodeAnalysis.CSharp` package version to 4.11.0.
 - Include `CHANGELOG.md` in NuGet package.

## 2.0.0.1 (2024-02-16)

 - Fix GitHub URL in NuGet package.

## 2.0.0 (2024-02-16)

 - Improve symbol matching in some circumstances.
 - Add option to exclude paths from the tree.
 - Project renamed (forced version increase to v2.0).

## 1.0.1 (2024-02-15)

 - Fix some generic method invocations being added to the tree twice.

## 1.0.0.2 (2024-02-12)

 - Update NuGet package metadata for publishing.

## 1.0.0.1 (2024-02-12)

 - Fix package binary name in README.
 - Remove `<br/>` tags in LICENSE.

## 1.0.0 (2024-02-12)

 - Initial public release.