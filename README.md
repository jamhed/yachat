YChat
=====

Yet another Erlang-based WebSockets Chat.

http://chat.jamhed.tk

INSTALL
=======

```
$ git submodule init
$ git submodule update
$ bower install
$ make
$ make console
```

TODO
====
5. On reconnects sometimes it says socket in connecting state

TODO NEXT
=========
0. On new user login/profile update notify user lists (lobby, convs)
1. Display uploaded file list
2. Get avatar from facebook
3. Auth with facebook (not using facebook id)
4. On click on #index no reload sometimes

2. Split to proper modules ws_ and db_
6. Count messages for diff. convs in UI
12. on leaving p2p conv destroy it?

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
30. File upload
31. file upload polishing: button css, type, content type, size -- separate table
32. user files list (by user_id, by type)
33. online user list
34. file upload button/css
35. file content-type
36. multiple file upload handling
37. notification message on file upload
38. User settings - get/set arbitrary attribute
39. proper conv_msg and handlers: conv/join CID, UID, conv/part CID, UID, user/p2p CID, UID, user/online CID, UID, user/offline CID, UID
40. File delete message
41. Filter messages for current conv in ConvText
42. user/update to use Jiffy built-in proplists
43. Conv users list view as Yavuz requested
44. Rescale avatars
45. Check convId for validity
46. Reworked config and logging modules
47. Separate config file (not sys.options)
48. Names for p2p confs
49. Stale session purge (logout/reconnect)
50. On logout purge session
