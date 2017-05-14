# 0.2.0.5 (2017-05-14)

	* Add `editorGeneric` and `editorGenericSimple` for types with generics-sop instances.
	The latter is only for record and newtypes, whereas the former supports also
	Union types, but comes with additional type class constraints.
	* Give `Editable` default implementations for generic types.
