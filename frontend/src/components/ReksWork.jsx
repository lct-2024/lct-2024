import React from 'react'
import style from "./ReksWork.module.css"

const ReksWork = () => {
    return (
        <div className='container'>
            <div className={style.main}>
                <h2>Уникальное отношение к процессу найма</h2>
                <div>
                    <div className={style.block}>
                        <div>
                            <h3>Вакансии - больше чем статичные документы с описанием</h3>
                            <h4>В Reksoft мы ценим открытость и прозрачность. Под каждой вакансией вы можете сразу задать вопросы о ней и получить ответы</h4>
                        </div>
                        <div>
                            <h3>фото</h3>
                        </div>
                        <div>
                            <h3>Нет отказов</h3>
                            <h4>Если у вас не хватает каких-либо компетенций, мы предложим рекомендации по их улучшению в учебных программах Академии Рексофт для последующего найма.                            </h4>
                        </div>
                        <div>
                            <h3>фото</h3>
                        </div>
                    </div>
                    <div className={style.block}>
                        <div>
                            <h3>фото</h3>
                        </div>
                        <div>
                            <h3>Структура управления предприятием ОТП Банка</h3>
                            <h4>Внедрение системы управления архитектурой предприятия для ОТП Банка</h4>
                        </div>

                        <div>
                            <h3>фото</h3>
                        </div>
                        <div>
                            <h3>DevOps для S7 Airlines</h3>
                            <h4>Автоматизация процессов DevOps для контентной платформы S7 Airlines. Разработка DevOps-платформы</h4>
                        </div>
                    </div>
                </div>
                <h2>преимущества работы в «рексофт»</h2>
                <div className={style.mainText}>
                    <div className={style.smallBlock}>
                        <div>
                            <p>Интересные задачи и передовые технологии</p>
                        </div>
                        <div>
                            <p>Команда профессионалов</p>
                        </div>
                        <div>
                            <p>Самореализация в крупной стабильной компании</p>
                        </div>
                        <div>
                            <p>Оформление и полис ДМС</p>
                        </div>
                        <div>
                            <p>Профессиональное обучение и сертификация</p>
                        </div>
                        <div>
                            <p>Гибкий график и возможность работать удаленно</p>
                        </div>
                    </div>
                    <div className={style.text}>
                        <p>«Рексофт» использует лучшие инструменты, чтобы расти, удивлять и развиваться.

                            ENTERPRISE BUSINESS APPLICATION:
                            Java EE, Spring, ActiveMQ, Liferay, RabbitMQ, Jaspersoft, Oracle, C#, PostgreSQL, Pentaho, Microsoft.NET, SQL Server, .NET Cоre, Visual Studio, Intellij Idea, Node.JS, Swagger, Freemarker.

                            FRONTEND:
                            HTML, CSS, Vue.js, JS, WebStorm, Jest, TypeScript, PHP, Angular, React, Redux, Next.js, Webpack.

                            MOBILE:
                            Android, iOS, Java, Objective-C, Kotlin, Swift, Android Studio, Xcode, React Native, SQLite, Firebase.
                        </p>
                    </div>
                </div>
            </div>
        </div>
    )
}

export default ReksWork