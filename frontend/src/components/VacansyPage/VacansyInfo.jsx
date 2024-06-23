import React, { useEffect, useState } from 'react';
import style from './VacansyInfo.module.css';
import Footer from '../Footer';
import Navigation from '../Navigation';
import { useParams } from 'react-router-dom';
import { useSelector, useDispatch } from 'react-redux';
import { fetchOrCreateChat, postMessage } from '../../store/commentsSlice';
import ApplyForm from './ApplyForm';
import Comments from '../Comments';
import axios from 'axios';

const VacansyInfo = ({ isHr }) => {
    const { id } = useParams();
    const dispatch = useDispatch();
    const [selectedFilter, setSelectedFilter] = useState("О проекте");
    const [btnText, setBtnText] = useState("Откликнуться");
    const [btnClicked, setBtnClicked] = useState(false);
    const [showAlarm, setShowAlarm] = useState(false);
    const [itsHr, setItsHr] = useState(false);

    const token = localStorage.getItem('authToken') || null;
    const vacansies = useSelector(state => state.vacansies.data);
    const vacansy = vacansies.find(v => v.id.toString() === id);
    const chatId = vacansy ? vacansy.chat_id : null;
    const comments = useSelector(state => state.comments.comments);
    const status = useSelector(state => state.comments.status);
    const error = useSelector(state => state.comments.error);

    useEffect(() => {
        if (token && chatId) {
            dispatch(fetchOrCreateChat({ contentId: id, contentType: 'vacancy' }));
        }
    }, [dispatch, token, chatId, id]);

    useEffect(() => {
        if (comments.length > 0) {
            setShowAlarm(true);
            setTimeout(() => {
                setShowAlarm(false);
            }, 2000);
        }
    }, [comments]);

    const handleFilterClick = (filter) => {
        setSelectedFilter(filter === selectedFilter ? null : filter);
    };

    const handleButtonClicked = async () => {
        if (btnClicked) return; // Do nothing if the button is already clicked

        setBtnClicked(true);
        setBtnText("Вы откликнулись");

        try {
            await axios.post(
                'https://ats.lct24.dev.40ants.com/api/apply_to_the_job',
                {
                    jsonrpc: '2.0',
                    method: 'apply_to_the_job',
                    params: {
                        job_id: vacansy.id
                    },
                    id: 1
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `${token}`,
                    },
                }
            );

            console.log('Отклик успешно отправлен');

        } catch (error) {
            console.error('Ошибка при отправке отклика:', error);
        }
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
                        <p> Языки: {vacansy.programming_languages.map(item => item.title).join(', ')}</p>
                        <p> Направление вакансии: {vacansy.category}</p>
                        <p> Навыки: {vacansy.skills.map(item => item.title).join(', ')}</p>
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
                            <button
                                style={{ opacity: btnClicked ? "0.5" : "1" }}
                                onClick={handleButtonClicked}
                                className={style.otklik}
                                disabled={btnClicked}
                            >
                                {btnText}
                            </button>
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
                        <Comments text="вакансии" key={vacansy.id} chatId={chatId} />
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
