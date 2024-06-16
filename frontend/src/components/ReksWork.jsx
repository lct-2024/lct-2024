import React, { useState } from 'react'
import style from "./ReksWork.module.css"

const ReksWork = () => {

    const [selectedFilter, setSelectedFilter] = useState("Интересные задачи и передовые технологии");

    const handleFilterClick = (filter) => {
        setSelectedFilter(filter === selectedFilter ? null : filter);
    };

    const getFilterText = () => {
        switch (selectedFilter) {
            case 'Интересные задачи и передовые технологии':
                return `«Рексофт» использует лучшие инструменты, чтобы расти, удивлять и развиваться. ENTERPRISE BUSINESS APPLICATION:
    Java EE, Spring, ActiveMQ, Liferay, RabbitMQ, Jaspersoft, Oracle, C#, PostgreSQL, Pentaho, Microsoft.NET, SQL Server, .NET Cоre, Visual Studio, Intellij Idea, Node.JS, Swagger, Freemarker.
    FRONTEND:   HTML, CSS, Vue.js, JS, WebStorm, Jest, TypeScript, PHP, Angular, React, Redux, Next.js, Webpack. MOBILE:
    Android, iOS, Java, Objective-C, Kotlin, Swift, Android Studio, Xcode, React Native, SQLite, Firebase.`;
            case 'Команда профессионалов':
                return `1) Не менее 10 сотрудников ежемесячно проходят обучение и сертификацию
    2) 18% сотрудников работают в компании более 10 лет 3) 40% сотрудников это seniors, leads, эксперты`;
            case 'Оформление и полис ДМС':
                return `1) Полностью официальное оформление 2) ДМС независимо от местонахождения
    3) 100% оплата больничных 4) Дополнительные опции ДМС по мере увеличения стажа работы в компании 5) Полис страхования путешествующих`;
            case 'Самореализация в крупной стабильной компании':
                return `Карьерная траектория 1)Индивидуальный план развития 2)Матрица компетенций Развитие сотрудников
1)Конференции 2)Сертификация 3)Обучение 4)Английский язык Обмен знаниями 1)Авторские курсы обучения 2)Внутренние вебинары`
            case 'Профессиональное обучение и сертификация':
                return `Развитие сотрудников - одна из основных ценностей "Рексофт". Наши сотрудники регулярно участвуют во внешних конференциях, в том числе в качестве спикеров и сдают экзамены на сертификацию. 
Мы с радостью рассмотрим возможность участия не только в представленных конференциях и обучении!
Конференции: JPOINT, HEISENBUG, TeamLead Conf, TestConference, НАЙТИ IT, HighLoad, RndTechConf, Joker, SECR, Frontend Conf, ANALYST DAYS, Mobius, ProfsoUX, SQA DAYS, HOLY JS.
Сертификаты: ISTQB, Microsoft, ORACLE, XIN, PMP, CSM Certification, Aws Certification.`
            case 'Гибкий график и возможность работать удаленно':
                return `Мы понимаем потребности кандидатов и учитываем пожелания наших сотрудников, поэтому «Рексофт» гибко подходит к вопросу графиков и к местонахождению сотрудников.
Расширяем команду, не ограничиваясь городами местонахождений наших офисов Даем сотрудникам возможность выбрать: работа из дома, в уютном коворкинге или за закрепленным рабочим местом
Каждый сотрудник может самостоятельно регулировать свой график работы`
            default:
                return '';
        }
    };

    return (
        <div className='container'>
            <div className={style.main}>
                <h2>Уникальное отношение к процессу найма</h2>
                <div className={style.blocks}>
                    <div className={style.block}>
                        <div>
                            <h3>Вакансии - больше чем статичные документы с описанием</h3>
                            <h4>В Reksoft мы ценим открытость и прозрачность. Под каждой вакансией вы можете сразу задать вопросы о ней и получить ответы</h4>
                        </div>
                        <div>
                            <img src="naym1.png" alt="фото" />
                        </div>
                        <div>
                            <h3>Нет отказов</h3>
                            <h4>Если у вас не хватает каких-либо компетенций, мы предложим рекомендации по их улучшению в учебных программах Академии Рексофт для последующего найма.                            </h4>
                        </div>
                        <div>
                            <img src="naym2.png" alt="фото" />
                        </div>
                    </div>
                    <div className={style.block}>
                        <div>
                            <img src="naym3.png" alt="фото" />
                        </div>
                        <div>
                            <h3>Подходящее резюме — наша забота</h3>
                            <h4>Каждый может улучшить свое резюме еще до отклика — мы сразу подсказываем насколько оно подходит вакансии и что можно исправить, чтобы подходить лучше</h4>
                        </div>

                        <div>
                            <img src="naym4.png" alt="фото" />
                        </div>
                        <div>
                            <h3>Выбираем по навыкам, а не быстроте отклика</h3>
                            <h4>Набираем пул кандидатов прежде, чем начать разбирать отклики для наиболее честного сравнения</h4>
                        </div>
                    </div>
                </div>
                <h2 className={style.fix}>Преимущества работы в «рексофт»</h2>
                <div className={style.mainText}>
                    <div className={style.smallBlock}>
                        <div className={selectedFilter === 'Интересные задачи и передовые технологии' ? style.activeFilter : ''} onClick={() => handleFilterClick('Интересные задачи и передовые технологии')}>
                            <p>Интересные задачи и передовые технологии</p>
                        </div>
                        <div className={selectedFilter === 'Команда профессионалов' ? style.activeFilter : ''} onClick={() => handleFilterClick('Команда профессионалов')}>
                            <p>Команда профессионалов</p>
                        </div>
                        <div className={selectedFilter === 'Самореализация в крупной стабильной компании' ? style.activeFilter : ''} onClick={() => handleFilterClick('Самореализация в крупной стабильной компании')}>
                            <p>Самореализация в крупной стабильной компании</p>
                        </div>
                        <div className={selectedFilter === 'Оформление и полис ДМС' ? style.activeFilter : ''} onClick={() => handleFilterClick('Оформление и полис ДМС')}>
                            <p>Оформление и полис ДМС</p>
                        </div>
                        <div className={selectedFilter === 'Профессиональное обучение и сертификация' ? style.activeFilter : ''} onClick={() => handleFilterClick('Профессиональное обучение и сертификация')}>
                            <p>Профессиональное обучение и сертификация</p>
                        </div>
                        <div className={selectedFilter === 'Гибкий график и возможность работать удаленно' ? style.activeFilter : ''} onClick={() => handleFilterClick('Гибкий график и возможность работать удаленно')}>
                            <p>Гибкий график и возможность работать удаленно</p>
                        </div>
                    </div>
                    <div className={style.text}>
                        <p>{getFilterText()}</p>
                    </div>
                </div>
            </div>
        </div>
    )
}

export default ReksWork