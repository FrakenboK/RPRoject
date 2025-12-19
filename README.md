# yisasnxlooukup

R-пакет для получения информации об IP-адресах через API ip-api.com с визуализацией в Shiny.

**GitHub:** [https://github.com/FrakenboK/RPRoject](https://github.com/FrakenboK/RPRoject)

## Установка

### Из GitHub

```r
remotes::install_github("FrakenboK/RPRoject")
```

**Примечание:** Если у вас не установлен пакет `remotes`, сначала установите его:

```r
install.packages("remotes")
```

### Из локальной директории (для разработки)

```r
devtools::install()
```

или

```r
devtools::load_all()
```

## Использование

После установки пакет импортируется стандартным способом:

```r
library(yisasnxlooukup)
```

### Быстрая установка и запуск

```r
# Установка из GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("FrakenboK/RPRoject")

# Загрузка пакета
library(yisasnxlooukup)

# Запуск приложения
run_app()
```

Доступные функции:
- `fetch_ip_api()` - получение данных об IP-адресах
- `prepare_ipinfo()` - нормализация данных
- `calc_ip_metrics()` - расчет метрик
- `run_app()` - запуск Shiny приложения

## Быстрый старт (R)

### Запуск Shiny приложения

```r
library(yisasnxlooukup)
run_app()
```

Или с параметрами:

```r
run_app(host = "0.0.0.0", port = 3838)
```

После запуска:
1. Введите IP-адреса в текстовое поле (по одному на строку)
2. Нажмите кнопку **"Fetch"** для получения данных через API
3. Результаты отобразятся в таблице и на графиках

**Примечание**: Кэш (кнопка "Load Cached") - опциональная функция. Основная работа идет через "Fetch" в UI.

### Использование API-функций (без UI)

```r
library(yisasnxlooukup)

ips <- c("8.8.8.8", "1.1.1.1")
raw <- fetch_ip_api(ips)
df <- prepare_ipinfo(raw)
metrics <- calc_ip_metrics(df)

print(metrics$summary)
print(metrics$by_country)
```

## Примечания

- При использовании функции `fetch_ip_api()` выполняются реальные HTTP-запросы к внешнему API ip-api.com
- Учтите лимиты внешнего API при работе с большим количеством IP-адресов
- Рекомендуется использовать опцию паузы между запросами для соблюдения rate limits

## Основные функции

| Функция | Описание |
|---------|----------|
| `fetch_ip_api(ips, ...)` | Получение данных об IP-адресах через API ip-api.com |
| `prepare_ipinfo(df_raw)` | Нормализация и очистка сырых данных |
| `calc_ip_metrics(df_clean)` | Расчет метрик и агрегатов (по странам, ASN) |
| `run_app(...)` | Запуск Shiny веб-приложения для визуализации |

### Примеры использования

```r
library(yisasnxlooukup)

# Получение информации об IP
result <- fetch_ip_api("8.8.8.8")

# Обработка данных
clean_data <- prepare_ipinfo(result)

# Расчет статистики
stats <- calc_ip_metrics(clean_data)
stats$summary
stats$by_country
stats$by_asn
```

## Два мира данных

Проект использует две зоны данных:

### 1) `raw-data/` (кухня для разработчика)
Эта папка коммитится в репозиторий и содержит:
- Исходные выгрузки и дампы (CSV, JSON)
- Промежуточные файлы
- R-скрипты подготовки данных
- Заметки и черновики

**Важно**: Содержимое `raw-data/` не используется напрямую Shiny приложением или дашбордом.

### 2) `data/` (рабочее хранилище рантайма)
Эта папка **не коммитится** (в `.gitignore`) и используется для:
- Кэширования результатов ETL
- Хранения обработанных данных (`.rds` файлы)
- Сохранения данных между перезапусками приложения

В Docker эта папка монтируется как volume: `./data:/srv/ourpack/data`

## Docker

### Запуск через Docker Compose

#### Запуск Shiny приложения (основной способ)
```bash
docker compose up app
```

Приложение будет доступно по адресу: http://localhost:3838

#### Запуск ETL (загрузка данных, опционально)
```bash
docker compose run --rm etl
```

Список IP можно задать через переменную окружения:
```bash
IP_LIST="8.8.8.8,1.1.1.1,208.67.222.222" docker compose run --rm etl
```

### Запуск через Docker (без compose)

#### Сборка образа
```bash
docker build -f docker/Dockerfile -t yisasnxlooukup .
```

#### Запуск ETL
```bash
docker run --rm -v $(pwd)/data:/srv/ourpack/data \
  -e OURPACK_DATA_DIR=/srv/ourpack/data \
  -e IP_LIST="8.8.8.8,1.1.1.1" \
  yisasnxlooukup Rscript docker/run_etl.R
```

#### Запуск Shiny приложения
```bash
docker run -p 3838:3838 \
  -v $(pwd)/data:/srv/ourpack/data \
  -e OURPACK_DATA_DIR=/srv/ourpack/data \
  yisasnxlooukup
```

## Использование кэша в Shiny

Shiny приложение автоматически пытается загрузить кэшированные данные при старте (если файлы существуют в `data/`).

Также доступна кнопка **"Load Cached"** для ручной загрузки данных из кэша.

Если кэша нет, приложение работает как обычно: ввод IP-адресов и кнопка "Fetch" для запроса данных через API.

