module DemoData exposing (..)

import Gql exposing (BookData)
import Graphql.Http
import LibraryApi.InputObject exposing (AuthorInput, BookInput)
import LibraryApi.Object.Book exposing (coverImageUrl)
import Task exposing (Task)


createAuthorAndBooks : AuthorInput -> (Int -> List BookInput) -> Task (Graphql.Http.Error Int) (List Int)
createAuthorAndBooks author fillBooks =
    Gql.createAuthorTask author
        |> Task.andThen (\authorId -> Task.sequence <| List.map Gql.createBookTask (fillBooks authorId))


loadDemoData : Task (Graphql.Http.Error ()) (List Gql.BookData)
loadDemoData =
    [ createAuthorAndBooks (AuthorInput "Айн Рэнд")
        (\id ->
            [ { title = "Источник"
              , coverImageSourcePath = "the_fountainhead.jpg"
              , authorId = id

              -- , topics = FICTION
              -- , url = "https://www.alpinabook.ru/catalog/book-66478/"
              -- , rating = Excellent
              }
            , { title = "Атлант расправил плечи"
              , coverImageSourcePath = "atlas_shrugged.jpg"
              , authorId = id

              -- , topics = FICTION
              -- , url = "https://www.alpinabook.ru/catalog/book-65392/"
              -- , rating = Excellent
              }
            , { title = "Ночью 16 января; Идеал; Подумай дважды"
              , coverImageSourcePath = "three_plays.jpg"
              , authorId = id

              -- , topics = FICTION
              -- , url = "https://ast.ru/book/nochyu-16-yanvarya-ideal-podumay-dvazhdy-028758/"
              -- , rating = WorthReading
              }
            , { title = "Гимн"
              , coverImageSourcePath = "anthem.jpg"
              , authorId = id

              -- , topics = FICTION
              -- , url = "https://www.alpinabook.ru/catalog/book-61335/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Aspen Pittman")
        (\id ->
            [ { title = "The Tube Amp Book - Deluxe Revised Edition"
              , coverImageSourcePath = "tube_amp.webp"
              , authorId = id

              -- , topics = MUSIC_INSTRUMENTS
              -- , url = "https://www.amazon.com/Tube-Amp-Book-Deluxe-Revised/dp/0879307676"
              -- , rating = NothingSpecial
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Donald Brosnac")
        (\id ->
            [ { title = "Guitar Electronics for Musicians"
              , coverImageSourcePath = "guitar_electronics.webp"
              , authorId = id

              -- , topics = MUSIC_INSTRUMENTS
              -- , url = "https://www.amazon.com/Guitar-Electronics-Musicians-Donald-Brosnac/dp/0711902321"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Hideo Kamimoto")
        (\id ->
            [ { title = "Complete Guitar Repair"
              , coverImageSourcePath = "guitar_repair.webp"
              , authorId = id

              -- , topics = MUSIC_INSTRUMENTS
              -- , url = "https://www.amazon.com/Complete-Guitar-Repair-Hideo-Kamimoto/dp/0825601568"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Roger H. Siminoff")
        (\id ->
            [ { title = "The Luthier's Handbook"
              , coverImageSourcePath = "luthiers_handbook.webp"
              , authorId = id

              -- , topics = MUSIC_INSTRUMENTS
              -- , url = "https://straightupstrings.com/collections/books-and-drawings/products/the-luthiers-handbook"
              -- , rating = Excellent
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Paul Balmer")
        (\id ->
            [ { title = "The Gibson Les Paul Handbook"
              , coverImageSourcePath = "lespaul_handbook.webp"
              , authorId = id

              -- , topics = MUSIC_INSTRUMENTS
              -- , url = "https://www.amazon.com/Gibson-Paul-Handbook-Maintain-Troubleshoot/dp/0760334706"
              -- , rating = NothingSpecial
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Vitaly Bragilevsky")
        (\id ->
            [ { title = "Haskell in Depth"
              , coverImageSourcePath = "haskell_in_depth.webp"
              , authorId = id

              -- , topics = HASKELL
              -- , url = "https://www.manning.com/books/haskell-in-depth"
              -- , rating = Excellent
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Дэйв Логан, Джон Кинг, Хэли Фишер-Райт")
        (\id ->
            [ { title = "Лидер и племя"
              , coverImageSourcePath = "leader.webp"
              , authorId = id

              -- , topics = LEADERSHIP
              -- , url = "https://www.mann-ivanov-ferber.ru/books/lider-i-plemya/"
              -- , rating = Excellent
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Фредерик Лалу")
        (\id ->
            [ { title = "Открывая организации будущего"
              , coverImageSourcePath = "reinventing_organizations.webp"
              , authorId = id

              -- , topics = LEADERSHIP
              -- , url = "https://www.mann-ivanov-ferber.ru/books/novyj-vzglyad-na-organizacii/"
              -- , rating = Excellent
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Питер Брегман")
        (\id ->
            [ { title = "Эмоциональная смелость"
              , coverImageSourcePath = "emotional.webp"
              , authorId = id

              -- , topics = LEADERSHIP
              -- , url = "https://www.mann-ivanov-ferber.ru/books/emoczionalnaya-smelost/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Джон Хеннесси")
        (\id ->
            [ { title = "Принципы лидера"
              , coverImageSourcePath = "leader_principles.webp"
              , authorId = id

              -- , topics = LEADERSHIP
              -- , url = "https://www.mann-ivanov-ferber.ru/books/princzipyi-lidera/"
              -- , rating = NothingSpecial
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Кейт Феррацци, Тал Рэз")
        (\id ->
            [ { title = "Никогда не ешьте в одиночку"
              , coverImageSourcePath = "never_eat_alone.webp"
              , authorId = id

              -- , topics = COMMUNICATION
              -- , url = "https://www.mann-ivanov-ferber.ru/books/mif/nevereatalone/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Джон Дорр")
        (\id ->
            [ { title = "Измеряйте самое важное"
              , coverImageSourcePath = "measure_what_matters.webp"
              , authorId = id

              -- , topics = MANAGEMENT
              -- , url = "https://www.mann-ivanov-ferber.ru/books/izmeryajte-samoe-vazhnoe/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Рэй Далио")
        (\id ->
            [ { title = "Принципы"
              , coverImageSourcePath = "dalio_principles.webp"
              , authorId = id

              -- , topics = LEADERSHIP
              -- , url = "https://www.mann-ivanov-ferber.ru/books/princzipyi/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Ричард Пулин")
        (\id ->
            [ { title = "Школа дизайна: макет"
              , coverImageSourcePath = "layout.webp"
              , authorId = id

              -- , topics = DESIGN
              -- , url = "https://www.mann-ivanov-ferber.ru/books/shkola-dizajna-maket/"
              -- , rating = Unknown
              }
            , { title = "Школа дизайна: шрифт"
              , coverImageSourcePath = "type.webp"
              , authorId = id

              -- , topics = DESIGN
              -- , url = "https://www.mann-ivanov-ferber.ru/books/shkola-dizajna-shrift/"
              -- , rating = NothingSpecial
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Максим Котин")
        (\id ->
            [ { title = "И ботаники делают бизнес 1+2"
              , coverImageSourcePath = "botaniki.webp"
              , authorId = id

              -- , topics = ENTREPRENEURSHIP
              -- , url = "https://www.mann-ivanov-ferber.ru/books/i_botaniki_delayut_biznes_2/"
              -- , rating = NothingSpecial
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Эмилио Пухоль")
        (\id ->
            [ { title = "Школа игры на шестиструнной гитаре"
              , coverImageSourcePath = "pujol.webp"
              , authorId = id

              -- , topics = MUSIC_INSTRUMENTS
              -- , url = "https://www.chitai-gorod.ru/catalog/book/358478/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Бернхард Ретцель")
        (\id ->
            [ { title = "Джентльмен. Путеводитель по стилю и моде для мужчин"
              , coverImageSourcePath = "gentleman.webp"
              , authorId = id

              -- , topics = FASHION
              -- , url = "https://www.ozon.ru/context/detail/id/4993260/"
              -- , rating = NothingSpecial
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Джон Чемберс")
        (\id ->
            [ { title = "Соединяя точки"
              , coverImageSourcePath = "connecting_dots.png"
              , authorId = id

              -- , topics = LEADERSHIP
              -- , url = "https://www.mann-ivanov-ferber.ru/books/soedinyaya-tochki/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Харвилл Хендрикс и Хелен Хант")
        (\id ->
            [ { title = "Любовь на всю жизнь"
              , coverImageSourcePath = "love_you_want.png"
              , authorId = id

              -- , topics = RELATIONS
              -- , url = "https://www.mann-ivanov-ferber.ru/books/lyubov-na-vsyu-zhizn/"
              -- , rating = Excellent
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Рада Агравал")
        (\id ->
            [ { title = "Вместе"
              , coverImageSourcePath = "together.png"
              , authorId = id

              -- , topics = COMMUNICATION
              -- , url = "https://www.mann-ivanov-ferber.ru/books/vmeste/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Эмбер Рэй")
        (\id ->
            [ { title = "Вся правда обо мне"
              , coverImageSourcePath = "truth_about_me.png"
              , authorId = id

              -- , topics = COMMUNICATION
              -- , url = "https://www.mann-ivanov-ferber.ru/books/vsya-pravda-obo-mne/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Авинаш Диксит и Барри Нейлбафф")
        (\id ->
            [ { title = "Теория игр"
              , coverImageSourcePath = "game_theory.png"
              , authorId = id

              -- , topics = MANAGEMENT
              -- , url = "https://www.mann-ivanov-ferber.ru/books/strategicheskoe_mishlenie/"
              -- , rating = Unknown
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Энн Джензер")
        (\id ->
            [ { title = "Как писать нон-фикшн"
              , coverImageSourcePath = "non_fiction.png"
              , authorId = id

              -- , topics = TEXT
              -- , url = "https://www.mann-ivanov-ferber.ru/books/kak-pisat-non-fikshn/"
              -- , rating = Unknown
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Нассим Николас Талеб")
        (\id ->
            [ { title = "Черный лебедь. Под знаком непредсказуемости"
              , coverImageSourcePath = "black_swan.jpg"
              , authorId = id

              -- , topics = MANAGEMENT
              -- , url = "https://azbooka.ru/books/chernyy-lebed-pod-znakom-nepredskazuemosti-2-e-izd-dopolnennoe-s1va"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Алекс Бэнкс, Ева Порселло")
        (\id ->
            [ { title = "GraphQL: язык запросов для современных веб-приложений"
              , coverImageSourcePath = "graphql.jpg"
              , authorId = id

              -- , topics = DEV
              -- , url = "https://www.piter.com/collection/programmirovanie-razrabotka-programnogo-obespecheniya/product/graphql-yazyk-zaprosov-dlya-sovremennyh-veb-prilozheniy"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Евгений Моргунов")
        (\id ->
            [ { title = "PostgreSQL. Основы языка SQL"
              , coverImageSourcePath = "postgres.jpg"
              , authorId = id

              -- , topics = DEV
              -- , url = "https://bhv.ru/product/postgresql-osnovy-yazyka-sql/"
              -- , rating = WorthReading
              }
            ]
        )
    , createAuthorAndBooks (AuthorInput "Алексей Локонцев")
        (\id ->
            [ { title = "История одного мирового бренда"
              , coverImageSourcePath = "topgun.jpg"
              , authorId = id

              -- , topics = ENTREPRENEURSHIP
              -- , url = "https://topgunbarber.ru/book/"
              -- , rating = WorthReading
              }
            ]
        )
    ]
        |> Task.sequence
        |> Task.mapError (Graphql.Http.mapError <| always ())
        |> Task.andThen (\_ -> Task.mapError (Graphql.Http.mapError <| always ()) Gql.allBooks)
