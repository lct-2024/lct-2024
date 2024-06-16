import React from 'react'
import style from "./OurProjects.module.css"
import { Link } from 'react-router-dom'

const OurProjects = () => {
    return (
        <div className='container'>
            <div className={style.main}>
                <h2>НАШИ ПРОЕКТЫ</h2>
                <div>
                    <div className={style.block}>
                        <div>
                            <h3>Система обработки багажа для аэропорта Домодедово</h3>
                            <h4>Автоматизированная система обработки багажа для аэропорта Домодедово</h4>
                            <p>Открытых вакансий: 1</p>
                            <button>Другое</button>
                        </div>
                        <div>
                            <h3>Автоматизация вопрос-ответ акционерного сообщества</h3>
                            <h4>Создание системы для автоматизации сессии вопрос-ответ на годовом собрании акционеров</h4>
                            <p>Открытых вакансий: 4</p>
                            <button>Другое</button>
                        </div>
                    </div>
                    <div className={style.block}>
                        <div>
                            <h3>Структура управления предприятием ОТП Банка</h3>
                            <h4>Внедрение системы управления архитектурой предприятия для ОТП Банка</h4>
                            <p>Открытых вакансий: 3</p>
                            <button>Другое</button>
                        </div>
                        <div>
                            <h3>DevOps для S7 Airlines</h3>
                            <h4>Автоматизация процессов DevOps для контентной платформы S7 Airlines. Разработка DevOps-платформы</h4>
                            <p>Проект завершен</p>
                            <button>Другое</button>
                        </div>
                    </div>
                </div>
                <Link to="/projects-page" className={style.btn}>все проекты <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path d="M5.00195 12H19.002M19.002 12L12.002 5M19.002 12L12.002 19" stroke="black" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                </svg>
                </Link>
                <h2>занимаемся цифровой трансформацией предприятий   в области промышленной автоматизации уже 30+ лет</h2>
                <div>
                    <div className={style.smallBlock}>
                        <div>
                            <p><span>600+ </span>
                                высококлассных профессионалов</p>
                        </div>
                        <div>
                            <p><span>№238 </span> из 479 в рейтинге работодателей hh</p>
                        </div>
                    </div>
                    <div className={style.smallBlock}>
                        <div>
                            <p><span>1000+ </span>
                                интересных проектов
                                в областях от финансов до ретейла</p>
                        </div>
                        <div>
                            <p><span>80% </span>
                                сотрудников
                                удовлетворены работой
                                в компании</p>
                        </div>
                    </div>
                </div>
                <button className={style.btn}>О компании <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path d="M5.00195 12H19.002M19.002 12L12.002 5M19.002 12L12.002 19" stroke="black" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                </svg>
                </button>
                <h2>“Рэксофт” - это про людей</h2>
                <div>
                    <div className={style.smallBlock2}>
                        <div>
                            <p>Стремимся к постоянному развитию каждого сотрудника и работе
                                с живыми тенденциями</p>
                        </div>
                        <div>
                            <img src="bl2.png" alt="фото" />
                        </div>
                        <div>
                            <p>Мы сочетаем свободу и правила, не перекрывая пути новым идеям и предложениям</p>
                        </div>
                        <div>
                            <img src="bl1.png" alt="фото" />
                        </div>
                    </div>
                    <div className={style.smallBlock2}>
                        <div>
                            <img src="bl3.png" alt="фото" />
                        </div>
                        <div>
                            <p>Каждый проект - вызов и желание создать нечто уникальное с полной самоотдачей</p>
                        </div>
                        <div>
                            <img src="bl4.png" alt="фото" />
                        </div>
                        <div>
                            <p>Для нас важны принципы внимания к каждому и слаженной командной работы. </p>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    )
}

export default OurProjects