Skip-List
=========

This package contains an implementation of an indexable skip-list

## TODO
* Doubly-linked list for reverse iteration
* Track last item in skip-list (analgous to a cursor...)
  * Sequence functions for subsequent insertions/deletions
  * Maintain n pointers for n cores - thread searches etc.
* Unroll lists
  * Reduce cache misses 50-80%
* Improve random generation (self-correcting)
  * Keep track of level distribution through deletions/insertions
  * Whichever group is over/underrepresented, target that level
* Perform maintenance operations during traversal

## Resources
http://opendatastructures.org/ods-java/4_3_SkiplistList_Efficient_.html