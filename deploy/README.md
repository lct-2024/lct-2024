# Как запустить сервис фронтенда на dev виртуалке

```bash
cd frontend
docker build . -t lct24-frontend

cd ../

ln -s /home/art/projects/lct-2024/deploy/.local/share/systemd/user/lct-2024.service ~/.local/share/systemd/user/lct-2024.service
systemctl --user daemon-reload
systemctl --user enable lct-2024
systemctl --user start lct-2024
```
