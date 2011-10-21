Erlang Trie Implementation
==========================

The data structure is only for storing keys as strings (lists of integers), but is able to get performance close to the process dictionary when doing key lookups (based [on results here](http://okeuday.livejournal.com/16941.html), [summarized here](http://okeuday.livejournal.com/17628.html), with [the benchmark here](http://github.com/okeuday/erlbench)).  So, this data structure is (currently) the quickest for lookups on key-value pairs where all keys are strings, if you ignore the process dictionary (which many argue should never be used).

The implementation stores leaf nodes as the string suffix because it is a [PATRICIA trie](http://xw2k.nist.gov/dads//HTML/patriciatree.html) (PATRICIA - Practical Algorithm to Retrieve Information Coded in Alphanumeric, D.R.Morrison (1968)).  Storing leaf nodes this way helps avoid single child leafs (compressing the tree a little bit).

The full OTP dict API is supported in addition to other functions.  Functions like foldl, iter, itera, and foreach traverse in alphabetical order.  Functions like map and foldr traverse in reverse alphabetical order.  There are also functions like find_prefix and is_prefix that check if a prefix exists within the trie.

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

BSD