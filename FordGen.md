---
project: XpfGeneral
src_dir: ./Source/Bases
src_dir: ./Source/CharConv
src_dir: ./Source/Collections
src_dir: ./Source/Containers
src_dir: ./Source/HashFunc
src_dir: ./Source/HighPrecision
src_dir: ./Source/InputNOutput
src_dir: ./Source/MemHandlers
src_dir: ./Source/RandGen
src_dir: ./Source/SortNRank
src_dir: ./Source/Strings
exclude_dir: ./Source/Bases/Core/Includes
exclude_dir: ./Source/Bases/PolyForms/GFORTRAN
exclude_dir: ./Source/Bases/Util/Includes
exclude_dir: ./Source/CharConv/Includes
exclude_dir: ./Source/Containers/DblLnkList/Includes
exclude_dir: ./Source/Containers/DynArray/Includes
exclude_dir: ./Source/Containers/RBTree/Includes
exclude_dir: ./Source/Containers/HashTable/Includes
exclude_dir: ./Source/Containers/PriorityQ/Includes
exclude_dir: ./Source/HashFunc/HashSecure/NIST Round2/Includes
exclude_dir: ./Source/MemHandlers/Includes
exclude_dir: ./Source/SortNRank/Includes
exclude_dir: ./Source/Strings/Tries_GFortran
output_dir: ./FordDoc/
summary: XpfLib - General Package Documentation
author: Niranama
extensions: f90
fpp_extensions: f90
preprocess: true
predocmark_alt: >
predocmark: %
docmark_alt: ^
docmark: !
display: public
max_frontpage_items: 25
source: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html

