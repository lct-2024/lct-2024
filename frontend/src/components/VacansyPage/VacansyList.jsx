import React, { useState } from 'react';
import style from './VacansyList.module.css';
import { useNavigate } from 'react-router-dom';
import { useSelector } from 'react-redux';

const VacansyList = ({ btnShow, hideBody, isHR }) => {
    const navigate = useNavigate();
    const vacansies = useSelector(state => state.vacansies.data);
    const [showCount, setShowCount] = useState(5);

    const handleVacancyClick = (vacansy) => {
        { isHR == null ? navigate(`/vacansy-info/${vacansy.id}`) : navigate(`/vacansy-info-hr/${vacansy.id}`) };
    };

    const handleLoadMore = () => {
        setShowCount(showCount + 5);
    };

    return (
        <div className='container'>
            <div className={style.list}>
                {vacansies.slice(0, showCount).map((vacansy) => (
                    <div key={vacansy.id} className={!hideBody ? style.hide : style.vacansy}
                        onClick={() => handleVacancyClick(vacansy)} style={{ cursor: {} }}>
                        <h2>{vacansy.title}</h2>
                        {!hideBody ? ""
                            : <>
                                <p>Зарплата: {vacansy.salary === null ? "Не указана" : vacansy.salary}</p>
                                <p>Дедлайн сбора откликов: {new Date(vacansy.active_to).toLocaleString()}</p>
                                <div className={style.body}>
                                    <div className={style.filter}>
                                        <p>{vacansy.type_of_employment}</p>
                                        <p>{vacansy.city}</p>
                                        <p>{vacansy.category}</p>
                                    </div>
                                    {!isHR ? <p>Совпадение с резюме {vacansy.resume_matching_score}%</p> : <p></p>}
                                </div>
                            </>}
                    </div>
                ))}
            </div>
            {!btnShow && <button onClick={handleLoadMore} className={style.btn}>Загрузить ещё</button>}
        </div >
    );
}

export default VacansyList;
