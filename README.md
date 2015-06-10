YChat
=====

Yet another Erlang-based WebSockets Chat.

http://chat.jamhed.tk

INSTALL
=======

```
$ git submodule update
$ bower install
$ make
$ make console
```

TODO
====

0. File upload
1. Check convId for validity
2. Split to proper modules
4. Separate config file (not sys.options)
6. Count messages for diff. convs
7. Filter messages for current conv in Dumper
11. Stale session (logout/reconnect)
12. on leaving p2p conv destroy it
13. proper sys_msg and handlers: conv/join CID, conv/part CID, user/p2p CID

DONE
====
1. 60-sec reconnect
2. Message timestamps
3. User registration with names and emails
4. User names
5. Error messages as dialogs
6. Leave conv
7. Check double joins to conv
8. Names in messages
9. Remove double buttons in templates and in places
10. On logout clear everything
11. On register auto-login
12. Refactor message/db to modules
13. User offline/online events
14. On online list all conv where we are in
15. Check database arguments for correctness (existance) - Guards
16. Common module for convId, userId, and displayName
17. List Conv users
18. Broadcast message user joining chat
19. Events with ID to filter replies
20. Double load conv by query_convs in Conv/List
21. Load/Edit/Update profile
22. Facebook register/login
23. Invite user to conv by name/email
24. Online status (connection on/off)
25. Session ids
26. Date of birth/City fields
27. Different user profiles -- full, short
28. P2P Chats
29. notify peer on p2p

