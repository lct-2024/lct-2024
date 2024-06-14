import React, { useState } from 'react';
import style from './UserProfile.module.css'
import { useSelector } from 'react-redux';
import Footer from '../Footer';
import Navigation from '../Navigation';

const UserProfile = () => {
    const user = useSelector((state) => state.auth.user);
    const [alerts, setAlerts] = useState([
        { rekName: "Белоусов Иван Петрович", type: "Отклик на вакансию", vacansyName: "Frontend-разработчик Vue", date: '14.06.24', deadline: '20.06.24' },
        { rekName: "Иванова Мария Сергеевна", type: "Приглашение на интервью", vacansyName: "Разработчик PLC", date: '17.06.24', deadline: '20.06.24' }
    ])
    const [showCount, setShowCount] = useState(3)

    const handleLoadMore = () => {
        setShowCount(showCount + 5);
    };


    if (!user) {
        return <div>Loading...</div>;
    }

    let classes = style.tgl + ' ' + style.tglLight
    console.log(classes)
    return (<>
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
                    <button className={style.btn2}>Редактировать</button>
                </div>
                <div className={style.settings}>
                    <div className={style.setTitle}>
                        <h2>Настройки уведомлений</h2>
                        <div className={style.checkboxWrapper}><input className={classes} id="cb1-6" type="checkbox" />
                            <label className={style.tglBtn} for="cb1-6" />
                            <p>Получать уведомления на почту</p>
                        </div>
                    </div>
                    <div className={style.setBody}>
                        <h2>Виды уведомлений приходящих на почту</h2>
                        <div>
                            <input type="checkbox" />
                            <p>Новые комментарии в обсуждениях, где вы участвуете</p>
                        </div>
                        <div>
                            <input type="checkbox" />
                            <p>Новые вакансии избранных проектов</p>
                        </div>
                        <div>
                            <input type="checkbox" />
                            <p>Изменение статуса откликов</p>
                        </div>
                    </div>
                </div>
                <div className={style.alerts}>
                    <p className={style.title}>Уведомления</p>
                    {alerts.slice(0, showCount).map((alert) => (
                        <div key={alert.id} className={style.alert}>
                            <div>
                                <p>{alert.rekName}</p>
                                <p>{alert.date}</p>
                            </div>
                            <p>{alert.type} {alert.vacansyName}</p>
                            <p>Дедлайн сбора откликов "{alert.vacansyName}": {alert.deadline}</p>
                            <button className={style.btn2}>Отозвать отклик</button>
                        </div>
                    ))}
                    <button onClick={handleLoadMore} className={style.btn2}>Загрузить ещё</button>
                </div>
            </div>
        </div >
        <Footer />
    </>
    );
};

export default UserProfile;
