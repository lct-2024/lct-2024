FROM node:14-alpine

# Устанавливаем рабочую директорию в Docker контейнере
WORKDIR /app

# Копируем package.json и package-lock.json в Docker контейнер
COPY package.json package-lock.json ./

# Устанавливаем зависимости приложения используя npm
RUN npm install

# Копируем файлы приложения в Docker контейнер
COPY . .

# Собираем React приложение
RUN npm run build

# Устанавливаем serve для запуска React приложения
RUN npm install -g serve

# Запускаем React приложение на порте 80
CMD ["serve", "-p", "80", "-s", "build"]