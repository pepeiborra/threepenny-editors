# 0.2.0.14 (2017-07-01)
    * Improved rendering of field names in generic editors.
# 0.2.0.13 (2017-06-22)
    * Added `liftEditor` to expose the underlying `Element` of an editor.
      This enables setting attributes in the element, including class and id.
# 0.2.0.12 (2017-06-21)
    * Export `EditorDef` and `EditorFactory` constructors to allow for
      wrapping of custom controls
# 0.2.0.11 (2017-06-10)
    * Documentation only release.
# 0.2.0.10 (2017-05-23)
    * Nested grids. All layouts are now grid based.
# 0.2.0.9 (2017-05-23)
    * Detect grid layouts and render them accordingly 
# 0.2.0.8 (2017-05-21)
    * Bug fixes
# 0.2.0.7 (2017-05-20)
    * Added `editorSelection`.
# 0.2.0.6 (2017-05-15)
    * Fix the `Editable` instance for `Identity` and remove reexports.
# 0.2.0.5 (2017-05-14)

	* Add `editorGeneric` and `editorGenericSimple` for types with generics-sop instances.
	The latter is only for record and newtypes, whereas the former supports also
	Union types, but comes with additional type class constraints.
	* Give `Editable` default implementations for generic types.
