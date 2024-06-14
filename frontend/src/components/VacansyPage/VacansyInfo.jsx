import React, { useEffect, useState } from 'react';
import style from './VacansyInfo.module.css';
import Footer from '../Footer';
import Navigation from '../Navigation';
import { useParams } from 'react-router-dom';
import { useSelector } from 'react-redux';
import ApplyForm from './ApplyForm';
import Comments from '../Comments';

const VacansyInfo = () => {
    const { id } = useParams();
    const vacansies = useSelector(state => state.vacansies.data);
    const vacansy = vacansies.find((v) => v.id.toString() === id);
    const [comments, setComments] = useState([
        { name: "Иванов Иван Иванович", text: "Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании." },
        { name: "Егоров Александр Петрович", text: "Здравствуйте! Можно ли совмещать работу с учебой?" }
    ]);
    const [newCommentText, setNewCommentText] = useState('');
    const [showInput, setShowInput] = useState(false);
    const [selectedFilter, setSelectedFilter] = useState("О проекте");
    const [btnText, setBtnText] = useState("Откликнуться");
    const [btnClicked, setBtnClicked] = useState(false);
    const [showAlarm, setShowAlarm] = useState(false);
    const [itsHr, setItsHr] = useState(false)

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
        setBtnClicked(true);
        setBtnText("Вы откликнулись");
    };

    const getFilterText = () => {
        switch (selectedFilter) {
            case 'О проекте':
                return (
                    <div className={style.desc}>
                        <h3>{vacansy.project.title}</h3>
                        <p>{vacansy.project.description}</p>
                    </div>
                );
            case 'Критерии':
                return (
                    <div className={style.desc}>
                        <p> Языки: {vacansy.programming_languages.map((item) => item.title).join(', ')}</p>
                        <p> Направление вакансии: {vacansy.category}</p>
                        <p> Навыки: {vacansy.skills.map((item) => item.title).join(', ')}</p>
                    </div>
                );
            case 'Обязанности':
                return (
                    <div className={style.desc}>
                        <p>{vacansy.description}</p>
                    </div>
                );
            case 'Условия':
                return (
                    <div className={style.desc}>
                        <p>Город: {vacansy.city}</p>
                        <p>Тип занятости: {vacansy.type_of_employment}</p>
                    </div>
                );
            default:
                return <div className={style.desc}></div>;
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

    if (!vacansy) {
        return <div>Вакансия не найдена</div>;
    }

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
                            <p>Вакансия создана: {new Date(vacansy.created_at).toLocaleString()}</p>
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
                        <Comments />
                    </div>
                    {itsHr && <ApplyForm jobId={vacansy.id} />}
                </div>
                {showAlarm && <p className={style.alarm}>Ваш отклик успешно отправлен! Уведомления об изменениях статуса отклика будут на вашей почте и на сайте</p>}
            </div>
            <Footer />
        </div>
    );
}

export default VacansyInfo;
