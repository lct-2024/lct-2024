import React, { useState } from 'react'
import style from "./VacansyPage.module.css"
import Navigation from '../Navigation'
import Footer from '../Footer'
import VacansyList from './VacansyList'

const VacansyPage = () => {

    const [vacansies, setVacansies] = useState([
        { title: "Разработчик PLC", salary: "Зарплата по итогам собеседования", date: "8 июня 2024", coincidence: 80 },
        { title: "Технический архитектор/ Эксперт по технологическим вопросам 1С", salary: "от 120 000 до 250 000 ₽", date: "10 июня 2024", coincidence: 65 },
        { title: "Разработчик Node.JS", salary: "от 350 000 до 500 000 ₽", date: "18 июня 2024", coincidence: 65 },
        { title: "Ивент-менеджер", salary: "Зарплата по итогам собеседования", date: "12 июня 2024", coincidence: 10 }
    ])
    const [searchTerm, setSearchTerm] = useState('');
    const [comments, setComments] = useState([
        { name: "Иванов Иван Иванович", text: "Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании." },
        { name: "Егоров Александр Петрович", text: "Здравствуйте! Можно ли совмещать работу с учебой?" }
    ])
    const [newCommentText, setNewCommentText] = useState('');
    const [showInput, setShowInput] = useState(false)

    const handleSearchChange = (e) => {
        setSearchTerm(e.target.value);
    };

    const handleSearchSubmit = () => {
        const filteredVacancies = vacansies.filter(vacansy =>
            vacansy.title.toLowerCase().includes(searchTerm.toLowerCase())
        );
        setVacansies(filteredVacancies);
    };

    const handleKeyPress = (event) => {
        if (event.key === 'Enter') {
            handleSearchSubmit()
            setSearchTerm("");
        }
    };

    const handleShowInput = () => {
        setShowInput(true);
    };

    const handleCommentSubmit = () => {
        setComments([
            ...comments,
            { name: "Николай Семенович", text: newCommentText }
        ]);
        setNewCommentText('');
        setShowInput(false);
    };


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
                        <input type="text" placeholder='Поиск...' value={searchTerm} onChange={handleSearchChange} onKeyPress={handleKeyPress} />
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
                        <button onClick={handleSearchSubmit}>
                            <svg width="25" height="25" viewBox="0 0 25 25" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M21.5 21.5L17.2 17.2M19.5 11.5C19.5 15.9183 15.9183 19.5 11.5 19.5C7.08172 19.5 3.5 15.9183 3.5 11.5C3.5 7.08172 7.08172 3.5 11.5 3.5C15.9183 3.5 19.5 7.08172 19.5 11.5Z" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                            </svg>
                        </button>
                    </div>
                </div>
                <VacansyList vacansies={vacansies} />
                <h2>Интересно узнать больше о вакансиях?</h2>
                <h2>Не нашли ответ на свой вопрос? Напишите в комментарии, <br /> чтобы получить ответ:</h2>
                <div className={style.comments}>
                    {comments.map((comment, i) => {
                        return <div className={style.comment} key={i}>
                            <div>
                                <h4>{comment.name}</h4>
                                <p>21.01.24  21.00</p>
                            </div>
                            <p>{comment.text}</p>
                        </div>
                    })}
                    {showInput && (<>
                        <textarea onChange={(e) => setNewCommentText(e.target.value)} value={newCommentText} placeholder='Комментарий...' />
                        <button className={style.btn} onClick={handleCommentSubmit}>
                            Отправить комментарий
                        </button>
                    </>)}
                    {showInput === false && <button className={style.btn} onClick={handleShowInput}>Написать комментарий</button>}
                </div>
            </div>
        </div>
        <Footer />
    </>
    )
}

export default VacansyPage