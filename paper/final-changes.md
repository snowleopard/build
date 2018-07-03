# An overview of changes in the final version

To address feedback from the reviewers as well as everyone else who reached out
to us, we have made the following changes:

* We substantially simplified our models of build systems (Section 5) by
finding a cleaner decomposition into two main components, which are now called a
'scheduler' and a 'rebuilder'. This allowed us to provide a complete model of
Excel (the submitted version omitted the implementation of the 'restarting'
scheduler), and to include models of CloudBuild, Buck and Nix.

* The decomposition forced us to introduce a new type `Task`, which describes a
single build task. The type corresponding to the description of all build tasks
is now appropriately called `Tasks` (Section 3).

* By popular demand we provided a model of Nix, a popular package manager/build
system, which required us to introduce a new type of traces -- 'deep
constructive traces' (Section 4). As a result, Table 2 (Build systems a la
carte) was extended with a new row.

* We made a lot of effort to improve text and clarity of descriptions.

