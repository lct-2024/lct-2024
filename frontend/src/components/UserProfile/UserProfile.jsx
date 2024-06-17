import React, { useEffect, useState } from 'react';
import style from './UserProfile.module.css';
import { useSelector, useDispatch } from 'react-redux';
import Footer from '../Footer';
import Navigation from '../Navigation';
import axios from 'axios';
import ModalUser from '../ModalWindows/ModalUser';
import { setUser } from '../../store/authSlice';

const UserProfile = () => {
    const dispatch = useDispatch();
    const user = useSelector((state) => state.auth.user);
    const [alerts, setAlerts] = useState([]);
    const [appliedJobs, setAppliedJobs] = useState([]);
    const [showCount, setShowCount] = useState(3);
    const [showModal, setShowModal] = useState(false);
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        axios.post('https://ats.lct24.dev.40ants.com/api/get_my_jobs', {
            jsonrpc: '2.0',
            method: 'get_my_jobs',
            params: {},
            id: 1
        },
            {
                headers: {
                    'Content-Type': 'application/json',
                    Authorization: `${localStorage.getItem('authToken')}`,
                }
            })
            .then(response => {
                setAppliedJobs(response.data.result || []);
                setLoading(false);
            })
            .catch(error => {
                console.error('Error fetching applied jobs:', error);
                setLoading(false);
            });
    }, []);

    const handleLoadMore = () => {
        setShowCount(showCount + 5);
    };

    const handleEditProfile = () => {
        setShowModal(true);
    };

    const handleCloseModal = () => {
        setShowModal(false);
    };

    const handleSaveProfile = (updatedUser) => {
        dispatch(setUser(updatedUser));
        handleCloseModal();
    };

    if (!user) {
        return <div>Loading...</div>;
    }

    let classes = style.tgl + ' ' + style.tglLight;

    return (
        <>
            <div className="container">
                <div className={style.main}>
                    <Navigation />
                    <div className={style.mainInfo}>
                        <div>
                            <p>Профиль</p>
                            <h1>{user.fio}</h1>
                            <h2>Дата рождения: {!user.dateB ? "Вы не указали дату" : user.dateB}</h2>
                            <h2>Почта: {user.email}</h2>
                            <h2>Номер телефона: {!user.phone ? "Вы не указали номер телефона" : user.phone}</h2>
                        </div>
                        <button className={style.btn2} onClick={handleEditProfile}>Редактировать</button>
                    </div>
                    <div className={style.settings}>
                        <div className={style.setTitle}>
                            <h2>Настройки уведомлений</h2>
                            <div className={style.checkboxWrapper}>
                                <input className={classes} id="cb1-6" type="checkbox" defaultChecked />
                                <label className={style.tglBtn} htmlFor="cb1-6" />
                                <p>Получать уведомления на почту</p>
                            </div>
                        </div>
                        <div className={style.setBody}>
                            <h2>Виды уведомлений приходящих на почту</h2>
                            <div>
                                <input type="checkbox" defaultChecked />
                                <p>Новые комментарии в обсуждениях, где вы участвуете</p>
                            </div>
                            <div>
                                <input type="checkbox" defaultChecked />
                                <p>Новые вакансии избранных проектов</p>
                            </div>
                            <div>
                                <input type="checkbox" defaultChecked />
                                <p>Изменение статуса откликов</p>
                            </div>
                        </div>
                    </div>
                    <div className={style.alerts}>
                        <p className={style.title}>Отклики</p>
                        {loading ? (
                            <p>Loading...</p>
                        ) : (
                            appliedJobs.length === 0 ? (
                                <p>Нет откликов на вакансии</p>
                            ) : (
                                appliedJobs.slice(0, showCount).map((job) => (
                                    <div key={job.id} className={style.alert}>
                                        <div>
                                            <p className={style.title2}>{job.title}</p>
                                            <p>Дедлайн: {job.active_to ? new Date(job.active_to).toLocaleString() : "Не указан"}</p>
                                        </div>
                                        <p>Проект: {job.project?.title}</p>
                                        <p>Город: {job.city}</p>
                                        <p>Вид занятости: {job.type_of_employment}</p>
                                        <p>Специализация: {job.programming_languages?.map((item) => item.title).join(', ')}</p>
                                        <div>
                                            <p>Зарплата: {job.salary ? job.salary : "Не указана"}</p>
                                            <p>Процент соответствия резюме: {job.resume_matching_score}%</p>
                                        </div>
                                    </div>
                                ))
                            )
                        )}
                        <button onClick={handleLoadMore} className={style.btn2}>Загрузить ещё</button>
                    </div>
                </div>
            </div>
            <Footer />
            {showModal && <ModalUser user={user} onClose={handleCloseModal} onSave={handleSaveProfile} />}
        </>
    );
};

export default UserProfile;
