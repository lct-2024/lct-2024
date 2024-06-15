import React, { useEffect, useState } from 'react';
import style from './ResumePage.module.css';
import Navigation from '../Navigation';
import Footer from '../Footer';
import Comments from '../Comments';
import ModalResume from '../ModalWindows/ModalResume';
import { useSelector, useDispatch } from 'react-redux';
import { fetchCVData, setShowModal, updateCVData } from '../../store/resumeSlice';

const ResumePage = () => {
    const [selectedFilter, setSelectedFilter] = useState("О себе");
    const [btnClicked, setBtnClicked] = useState(false);
    const [showAlarm, setShowAlarm] = useState(false);
    const dispatch = useDispatch();
    const { cvData, showModal } = useSelector((state) => state.resume);
    const token = localStorage.getItem('authToken');

    // Fetch CV data on component mount or when token changes
    useEffect(() => {
        if (token) {
            dispatch(fetchCVData(token));
        }
    }, [dispatch, token]);

    // Update local component state when cvData changes
    useEffect(() => {
        if (cvData) {
            setSelectedFilter("О себе"); // Reset selected filter
        }
    }, [cvData]);

    const handleButtonClicked = () => {
        dispatch(setShowModal(true));
    };

    const handleUpdateCV = (updatedCV) => {
        dispatch(updateCVData({ token, cvData: updatedCV }));
    };

    const getFilterText = () => {
        if (!cvData) {
            return <p>Резюме еще не создано</p>;
        }
        switch (selectedFilter) {
            case 'О себе':
                return (
                    <div className={style.desc}>
                        <h3>{cvData.name || 'Название не указано'}</h3>
                        <p>{cvData.about || 'Описание не указано'}</p>
                    </div>
                );
            case 'Навыки':
                return (
                    <div className={style.desc}>
                        {cvData?.skills && cvData.skills.map((skill, index) => (
                            <p key={index}>{skill}</p>
                        ))}
                    </div>
                );
            case 'Опыт работы':
                return (
                    <div className={style.desc}>
                        <p>{cvData?.experience || 'Опыт работы не указан'}</p>
                    </div>
                );
            case 'Образование':
                return (
                    <div className={style.desc}>
                        {cvData?.education && cvData.education}
                    </div>
                );
            default:
                return <div className={style.desc}></div>;
        }
    };

    return (
        <div className={style.main}>
            <div className='container'>
                <div className={style.mainBlock}>
                    <Navigation />
                    <div className={style.blocks}>
                        <div className={style.body}>
                            <p>Резюме</p>
                            <h2>{cvData?.name || 'Название не указано'}</h2>
                            <p>Желаемая зарплата: {cvData?.salary || 'не указана'}</p>
                            <p>Ссылка на портфолио: {cvData?.portfolio || 'не указана'}</p>
                            <div className={style.filter}>
                                <p>Разработка</p>
                            </div>
                            <button style={{ opacity: btnClicked ? "0.5" : "1" }} onClick={handleButtonClicked} className={style.otklik}> {cvData ? 'Редактировать' : 'Создать резюме'}</button>
                        </div>
                        <div className={style.body2}>
                            <p>Ваше резюме заполнено</p>
                            <p>на {cvData ? Math.round((Object.keys(cvData).length / 15) * 100) : 0}%</p>
                            <p className={style.light}>Подсветить недостающие пункты</p>
                        </div>
                    </div>
                    <div className={style.block}>
                        <div className={style.smallBlock}>
                            <div className={selectedFilter === 'О себе' ? style.activeFilter : ''}
                                onClick={() => setSelectedFilter('О себе')}>
                                <p>О себе</p>
                            </div>
                            <div className={selectedFilter === 'Навыки' ? style.activeFilter : ''}
                                onClick={() => setSelectedFilter('Навыки')}>
                                <p>Навыки</p>
                            </div>
                            <div className={selectedFilter === 'Опыт работы' ? style.activeFilter : ''}
                                onClick={() => setSelectedFilter('Опыт работы')}>
                                <p>Опыт работы</p>
                            </div>
                            <div className={selectedFilter === 'Образование' ? style.activeFilter : ''}
                                onClick={() => setSelectedFilter('Образование')}>
                                <p>Образование</p>
                            </div>
                        </div>
                        <div className={style.text}>
                            <p>{getFilterText()}</p>
                        </div>
                    </div>
                    <div className={style.lastSect}>
                        <Comments text="том как правильно заполнить резюме" />
                    </div>
                </div>
                {showAlarm && <p className={style.alarm}>Ваша вакансия успешно изменена!</p>}
            </div>
            <Footer />
            {showModal && <ModalResume onSave={handleUpdateCV} />}
        </div>
    );
}

export default ResumePage;
