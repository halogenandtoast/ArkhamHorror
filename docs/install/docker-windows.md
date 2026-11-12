# Installing with Docker on Windows

- Install Docker, specifically the version of Docker known as [Docker Desktop](https://docs.docker.com/desktop/).
- Install a "terminal only" version of Ubuntu that can be run inside of Windows, for which the easiest available method is to [install from the Microsoft Store](https://documentation.ubuntu.com/wsl/latest/howto/install-ubuntu-wsl2/#method-3-install-from-the-microsoft-store). You may choose any Linux distro other than Ubuntu if you know what you are doing.
- In Docker Desktop, go to `Settings > Resources > WSL Integration` and enable integration with Ubuntu Linux distro that you installed in the previous step. This allows us to access Docker from inside Ubuntu.
- Start the Ubuntu terminal and run:
  ```
  curl -fsSL https://raw.githubusercontent.com/halogenandtoast/ArkhamHorror/main/install.sh | bash
  ```
- The app should start being served at http://localhost:3000, if nothing went wrong.
- Every time you want to start the app, you can use `docker compose up`
- If you get around to figuring out how to use Docker Desktop, you could also press buttons to start and stop the app instead of typing commands to run it.

## Things that can go wrong

If the app doesnt start at all, or it starts but you cannot create a user account, or any other issue due to Murphy's Law, you have to check the logs of the Docker container to find the error messages. In Docker Desktop, you can access this by clicking on the `arkham-horror` "compose stack" found in the `Containers` list.

Possible problems:

| Problem | Solution |
| ------------- | ------------- |
| Error involving `docker-credential-desktop.exe` | Try restarting your computer |
| When clicking on the "REGISTER" button, the error `password authentication failed` is logged| Stop the app using `docker compose down` and then refetch the app using `docker compose pull`

The [README](../../README.md#i-just-want-to-try-this-out-on-my-computer)
has the other ways to install it.
