---
editor_options: 
  markdown: 
    wrap: sentence
---

## Contributing

This document outlines how to propose a change to the TRM Calculator application.

### Filing an issue

The first step in proposing a change to the application is to [file an issue on GitHub](https://github.com/FredHutch/trm-calculator/issues/new).

-   **If you are suggesting a new feature to be added to the application**, please file an issue that describes the scope of the addition, the benefit of its inclusion, and any potential drawbacks.

-   **If you have found a bug in the application**, please file an issue that describes the problem with a sufficient amount of detail for it to be reproduced.

Once an issue has been filed, the next step is to create a corresponding pull request (PR).

### Submitting a pull request

-   Fork from the `dev` branch of the `trm-calculator` repository and clone it onto your computer.

-   Create a new Git branch for your pull request (PR).

-   Make your changes, commit to git, and then create a pull request.

    -   The title of your PR should briefly describe the change.

    -   Make sure to link the PR to the corresponding issue that it addresses.

    -   Ensure that your PR is submitted against the `dev` branch and not against any other currently active branches.

### Code style

-   Any new code that is written should follow the [Fred Hutch WILDS Contributor style guide](https://getwilds.org/guide/style.html).

### Other notes

-   Each issue / PR that is filed for the application should correspond to a single feature or update.
    Never combine multiple changes in a single PR.

-   Once the PR has been submitted, a member of the DaSL team will review the proposed changes and potentially request further updates before accepting the request and merging the code into `dev`.
