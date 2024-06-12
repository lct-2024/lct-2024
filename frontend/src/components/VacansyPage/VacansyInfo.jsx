import React, { useEffect, useState } from 'react'
import style from './VacansyInfo.module.css'
import Footer from '../Footer'
import Navigation from '../Navigation'
import { useParams } from 'react-router-dom'
import ApplyForm from './ApplyForm'

const VacansyInfo = ({ vacansies }) => {
    const { id } = useParams();
    const vacansy = vacansies.find((v) => v.id.toString() === id);
    const [comments, setComments] = useState([
        { name: "Иванов Иван Иванович", text: "Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании." },
        { name: "Егоров Александр Петрович", text: "Здравствуйте! Можно ли совмещать работу с учебой?" }
    ])
    const [newCommentText, setNewCommentText] = useState('');
    const [showInput, setShowInput] = useState(false)
    const [selectedFilter, setSelectedFilter] = useState("О проекте");
    const [btnText, setBtnText] = useState("Откликнуться")
    const [btnClicked, setBtnClicked] = useState(false)
    const [showAlarm, setShowAlarm] = useState(false);
    const handleFilterClick = (filter) => {
        setSelectedFilter(filter === selectedFilter ? null : filter);
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

    const handleButtonClicked = () => {
        setBtnClicked(true)
        setBtnText("Вы откликнулись")
    }

    const getFilterText = () => {
        switch (selectedFilter) {
            case 'О проекте':
                return <div className={style.desc}>
                    <h3>{vacansy.project.title}</h3>
                    <p>{vacansy.project.description}</p>
                </div>;
            case 'Критерии':
                return <div className={style.desc}>
                    <p> Языки: {vacansy.programming_languages.map((item) => item.title).join(', ')}</p>
                    <p> Опыт работы: { }</p>
                    <p> Навыки: {vacansy.skills.map((item) => item.title).join(', ')}</p>
                </div>;
            case 'Обязанности':
                return <div className={style.desc}>
                    <p>{vacansy.description}</p>
                </div>;
            case 'Условия':
                return <div className={style.desc}>
                    <p>Город: {vacansy.city}</p>
                    <p>Тип занятости: {vacansy.type_of_employment}</p>
                </div>;
            default:
                return <div className={style.desc}>

                </div>;
        }
    };


    useEffect(() => {
        if (btnClicked) {
            setShowAlarm(true);
            setTimeout(() => {
                setShowAlarm(false);
            }, 2000);
        }
    }, [btnClicked]);

    return (
        <div className={style.main}>
            <div className='container'>
                <div className={style.mainBlock}>
                    <Navigation />
                    <div className={style.blocks}>
                        <div className={style.body}>
                            <p>Вакансия</p>
                            <h2>{vacansy.title}</h2>
                            <p>Зарплата: {vacansy.salary === null ? "Не указана" : vacansy.salary}</p>
                            <p>Дедлайн сбора откликов: {new Date(vacansy.active_to).toLocaleString()}</p>
                            <div className={style.filter}>
                                <p>{vacansy.type_of_employment}</p>
                                <p>{vacansy.city}</p>
                                <p>{vacansy.category}</p>
                            </div>
                            <button style={{ opacity: btnClicked ? "0.5" : "1" }} onClick={handleButtonClicked} className={style.otklik}>{btnText}</button>
                        </div>
                        <div className={style.body2}>
                            {vacansy.resume_matching_score > 40 ? <p>Ваше резюме подходит под описание вакансии</p> : <p>Ваше резюме не подходит под описание вакансии</p>}
                            <p>Требуемые навыки: 0/8</p>
                            <p>Опыт работы Х</p>
                            <p className={style.light}>Подсветить недостающие пункты</p>
                        </div>
                    </div>
                    <div className={style.block}>
                        <div className={style.smallBlock}>
                            <div className={selectedFilter === 'О проекте' ? style.activeFilter : ''}
                                onClick={() => handleFilterClick('О проекте')}>
                                <p>О проекте</p>
                            </div>
                            <div className={selectedFilter === 'Критерии' ? style.activeFilter : ''}
                                onClick={() => handleFilterClick('Критерии')}>
                                <p>Критерии</p>
                            </div>
                            <div className={selectedFilter === 'Обязанности' ? style.activeFilter : ''}
                                onClick={() => handleFilterClick('Обязанности')}>
                                <p>Обязанности</p>
                            </div>
                            <div className={selectedFilter === 'Условия' ? style.activeFilter : ''}
                                onClick={() => handleFilterClick('Условия')}>
                                <p>Условия</p>
                            </div>
                            <div className={selectedFilter === 'Команда' ? style.activeFilter : ''}
                                onClick={() => handleFilterClick('Команда')}>
                                <p>Команда</p>
                            </div>
                        </div>
                        <div className={style.text}>
                            <p>{getFilterText()}</p>
                        </div>
                    </div>
                    <div className={style.lastSect}>
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
                    <ApplyForm jobId={vacansy.id} />
                </div>
                {showAlarm && <p className={style.alarm}>Ваш отклик успешно отправлен! Уведомления об изменениях статуса отклика будут на вашей почте и на сайте</p>}
            </div>
            <Footer />
        </div >
    )
}

export default VacansyInfo