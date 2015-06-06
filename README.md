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

1. Online status (connection on/off)
2. Load/Edit/Update profile
3. Facebook auth
4. Separate config file (not sys.options)
5. P2P Chats
6. Count messages for diff. convs
7. Filter messages for current conv in Dumper
8. Inite user by name/email

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

