#name : for (Idx = 0 ; Idx < N ; ++Idx) { ... }
# --
for (${1:Idx}${2: = 0} ; $1${3: < N} ; ${4:++}$1)
{
    $0
}