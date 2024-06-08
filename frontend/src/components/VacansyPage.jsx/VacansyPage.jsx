import React from 'react'
import style from "./VacansyPage.module.css"
import Navigation from '../Navigation'
import Footer from '../Footer'
import VacansyList from './VacansyList'

const VacansyPage = () => {
    return (<>
        <div className={style.main}>
            <div className='container'>
                <div className={style.head}>
                    <Navigation />
                    <h1>ВАКАНСИИ</h1>
                </div>
            </div>
        </div>
        <div className='container'>
            <div className={style.body}>
                <div>
                    <div className={style.search}>
                        <input type="text" placeholder='Поиск...' />
                        <div>Все категории
                            <svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M6 7L12 13L18 7" stroke="black" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                            </svg>
                        </div>
                        <div>Все специальности<svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                            <path d="M6 7L12 13L18 7" stroke="black" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                        </svg>
                        </div>
                        <div>Все города<svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                            <path d="M6 7L12 13L18 7" stroke="black" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                        </svg>
                        </div>
                        <div>Все проекты<svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                            <path d="M6 7L12 13L18 7" stroke="black" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                        </svg>
                        </div>
                        <button>
                            <svg width="25" height="25" viewBox="0 0 25 25" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M21.5 21.5L17.2 17.2M19.5 11.5C19.5 15.9183 15.9183 19.5 11.5 19.5C7.08172 19.5 3.5 15.9183 3.5 11.5C3.5 7.08172 7.08172 3.5 11.5 3.5C15.9183 3.5 19.5 7.08172 19.5 11.5Z" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                            </svg>
                        </button>
                    </div>
                </div>
                <VacansyList />
                <h2>Интересно узнать больше о вакансиях?</h2>
                <h2>Не нашли ответ на свой вопрос? Напишите в комментарии, <br /> чтобы получить ответ:</h2>
                <div className={style.comments}>
                    <div className={style.comment}>
                        <div>
                            <h4>Иванов Иван Иванович</h4>
                            <p>21.01.24  21.00</p>
                        </div>
                        <p>Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании.</p>
                    </div>
                    <div className={style.comment}>
                        <div>
                            <h4>Егоров Александр Петрович</h4>
                            <p>21.01.24  21.00</p>
                        </div>
                        <p>Здравствуйте! Можно ли совмещать работу с учебой?</p>
                    </div>
                    <button className={style.btn}>Написать комментарий</button>
                </div>
            </div>
        </div>
        <Footer />
    </>
    )
}

export default VacansyPage