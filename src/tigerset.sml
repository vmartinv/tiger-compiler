structure tigerset:> tigerset =
struct


type 'item set = 'item Splayset.set ref

fun emptySet(cmp) = ref (Splayset.empty cmp)

fun listToSet(l, cmp) = ref (Splayset.addList(Splayset.empty cmp, l))

end
