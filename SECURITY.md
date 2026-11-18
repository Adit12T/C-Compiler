# Security policy

## Reporting a vulnerability

Please report security problems privately, not in a public issue.

- Preferred: open the **Security** tab of this repository and choose **Report a vulnerability**.
- Otherwise: send me a message at https://www.linkedin.com/in/aditya12t.

Include what you found, how to reproduce it, and the impact you expect. I aim to reply within 7 days.

## Scope

This is a portfolio project. It ships with synthetic sample data only and holds no real
customer data, credentials or secrets. Configuration secrets are read from environment variables.

## How this repository is protected

- Code scanning (CodeQL) on every push, every pull request and weekly.
- Dependabot checks dependencies and GitHub Actions for updates.
- Secret scanning with push protection blocks committed credentials.
- Workflows run with read-only repository permissions.
