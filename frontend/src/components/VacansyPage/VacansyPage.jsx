import React, { useState, useEffect } from 'react'
import style from "./VacansyPage.module.css"
import Navigation from '../Navigation'
import Footer from '../Footer'
import VacansyList from './VacansyList'
import axios from 'axios'

const VacansyPage = ({ vacansies, setVacansies }) => {

    useEffect(() => {
        const fetchData = async () => {
            try {
                const response = await axios.post('https://ats.lct24.dev.40ants.com/api/get_jobs', {
                    jsonrpc: '2.0',
                    method: 'get_jobs',
                    params: [],
                    id: 1
                });
                if (response.data.error) {
                    console.error('Error fetching data:', response.data.error.message);
                } else {
                    console.log(response.data.result)
                    setVacansies(response.data.result);
                }

            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };

        fetchData();
    }, [setVacansies]);
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


    return (<section>
        <div className={style.main}>
            <div className='container'>
                <div className={style.head}>
                    <Navigation />
                    <h1>ВАКАНСИИ</h1>
                </div>
            </div>
        </div>
        <div className={style.body}>
            <div className="container">
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
            {vacansies.length === 0 ? <h1 style={{ margin: "0 auto" }}>Загрузка...</h1> : <VacansyList vacansies={vacansies} />}


            <div className='container'>
                <div>
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
        </div>
    </section >
    )
}

export default VacansyPage