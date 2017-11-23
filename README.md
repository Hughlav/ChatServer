# Chatserver

This chatserver is for module CS4400 taught by Stephen Barrett. When tested against the test server im getting about 64%. The problem is with the leave message not being sent (with correct roomID) to all the clients in the rooms of the leaving clients. My comments in the code indicate where I think it is going wrong. Due to time constraints I wont be able to fix this now but hope to come back to this. 

Things to note:
This was my first attempt at writing haskell code. Because of this I based the model off a re-implimentation of Simon Marlow's Async Haskell Chat Server with changes and additions to suit this assignment. For the next project, implementing a distributed file system, I am building my own model.

The crazy number of commits with minor changes is due to the fact the test server was sitting on a server in the colleges SCSS network and my server had to be on the network to test. I was doing a lot of my programming at home while ssh into the SCSS network. So i would make a change, commit it and then pull it down on the machine i was ssh'd into. I ended up using nano editor to avoid doing all the commits.