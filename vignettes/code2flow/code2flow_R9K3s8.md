# Pseudocode for code2flow

Can be accessed [here](https://app.code2flow.com/nboDrmgQxXvp).

```
initialise `collEvents`;
for(`frame = 1`; `frame <= max`; `frame++`) {
get objects
from a frame;

if (frame has objects) {
if(`collEvents` empty) {
identify `newCollEvents`;
`collEvents += newCollEvents`;
} else {
get `collEvents(T-1)`;

if (collEvents exist @ T-1) {
cartesian join of
objects @ T 
and objects in 
`collEvents(T-1)`;
compute `dist`
between 
objects @ T
and objects in
`collEvents(T-1)`;

if (`dist < eps`) {
add objects 
to `collEvents`;
} else {
identify `newCollEvents`;
`collEvents += newCollEvents`;
}
} else {
identify `newCollEvents`;
`collEvents += newCollEvents`;
}
}
}
}
end;
```
