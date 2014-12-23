Thoughts on documents:

Documents need to keep a (limited?) history of their edits, as well as the current state of the document (snapshot?). They should be able to identify the parent of an incoming edit, get all edits which have occured since that parent, and transform the incoming edits against all those edits. Finally the now transformed edit should be appended to the history, and the document should have a new state.

History is not a buffer because it doesn't indicate things that are waiting to be sent. It indicates things that are waiting to be logged against.

data that a document needs to know?

- history
- current value
- max history size? 
- peers?

Is there a special history protocol/type that needs to exist so that we can separate having a history from having a specific value? Probably? 

Should transform-incoming be done by the snapshot or should it be done by the history? Probably the history.

Ideal data flow:

Document is created
A connection is created, meaning we need to keep history?
we have a map of connections to the time they open a historical copy and if the version falls outside of the historical size, then we should close the connection. Then on the client, if we recognize that a connection has been closed by the server we should reopen the connection and reload with the new state.
Alternatively, we could throw an error if the version is older than the max version?
transact!/update!/permutate!
