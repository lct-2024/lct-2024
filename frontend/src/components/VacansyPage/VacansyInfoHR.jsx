import React, { useEffect, useState } from 'react';
import style from './VacansyInfoHR.module.css';
import Footer from '../Footer';
import Navigation from '../Navigation';
import { useParams } from 'react-router-dom';
import { useSelector, useDispatch } from 'react-redux';
import { fetchOrCreateChat, postMessage } from '../../store/commentsSlice';
import axios from 'axios';

const VacansyInfoHR = () => {
    const { id } = useParams();

    const token = localStorage.getItem('authToken') || null;
    const vacansies = useSelector(state => state.vacansies.data);
    const vacansy = vacansies.find(v => v.id.toString() === id);
    const [resumes, setResumes] = useState([])

    useEffect(() => {
        const fetchData = async () => {
            try {
                const response = await axios.post('https://ats.lct24.dev.40ants.com/api/get_applicants', {
                    jsonrpc: '2.0',
                    method: 'get_applicants',
                    params: {
                        job_id: vacansy.id
                    },
                    id: 1
                });
                if (response.data.error) {
                    console.error('Error fetching data:', response.data.error.message);
                } else {
                    console.log(response.data.result)
                    setResumes(response.data.result)
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };

        fetchData();
    }, []);

    if (!vacansy) {
        return <div>Вакансия не найдена</div>;
    }

    return (
        <div className={style.main}>
            <div className='container'>
                <div className={style.mainBlock}>
                    <Navigation />
                    <div className={style.blocks}>
                        {resumes.map((resume, i) => (
                            <div className={style.mainBlocks} key={i}>
                                <h2>{resume.title}</h2>
                                {resume.applicants && resume.applicants.length > 0 ? (
                                    <div>
                                        <div className={style.header}>
                                            <p className={style.fio}>ФИО</p>
                                            <p className={style.step}>Этапы</p>
                                            <p className={style.score}>Совпадение</p>
                                            <p className={style.date}>Дата отклика</p>
                                        </div>
                                        {resume.applicants.map((applicant, j) => (
                                            <div key={j}>
                                                <div className={style.block}>
                                                    <p className={style.fio}>{applicant.fio}</p>
                                                    {applicant.step ? <p className={style.step}>{applicant.step}</p> : "Информация не найдена"}
                                                    <p className={style.score}> {applicant.score}%</p>
                                                    <p className={style.date}> {applicant.date}</p>
                                                </div>
                                            </div>
                                        ))}
                                    </div>
                                ) : (
                                    <p>Нет заявок</p>
                                )}
                            </div>
                        ))}
                    </div>
                </div>
            </div>
            <Footer />
        </div>
    );
}

export default VacansyInfoHR;
