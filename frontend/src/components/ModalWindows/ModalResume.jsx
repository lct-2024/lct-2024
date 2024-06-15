import React, { useState, useEffect } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { fetchCVData, updateCVData, setShowModal } from '../../store/resumeSlice';
import style from './ModalResume.module.css';

const ModalResume = () => {
    const dispatch = useDispatch();
    const { cvData, showModal } = useSelector((state) => state.resume);
    const token = localStorage.getItem('authToken');

    const [name, setName] = useState('');
    const [about, setAbout] = useState('');
    const [experience, setExperience] = useState('');
    const [skills, setSkills] = useState([]);
    const [education, setEducation] = useState([]);
    const [salary, setSalary] = useState('');
    const [portfolio, setPortfolio] = useState('');

    useEffect(() => {
        if (cvData) {
            setName(cvData.name || '');
            setAbout(cvData.about || '');
            setExperience(cvData.experience || '');
            setSkills(cvData.skills || []);
            setEducation(cvData.education || []);
            setSalary(cvData.salary || '');
            setPortfolio(cvData.portfolio || '');
        }
    }, [cvData]);

    const handleInputChange = (e) => {
        const { name, value } = e.target;
        switch (name) {
            case 'name':
                setName(value);
                break;
            case 'about':
                setAbout(value);
                break;
            case 'experience':
                setExperience(value);
                break;
            case 'salary':
                setSalary(value);
                break;
            case 'portfolio':
                setPortfolio(value);
                break;
            default:
                break;
        }
    };

    const addSkill = () => {
        const newSkill = prompt('Добавьте новый навык:');
        if (newSkill) {
            setSkills([...skills, newSkill]);
        }
    };

    const addEducation = () => {
        const newEducation = prompt('Введите данные об образовании:');
        if (newEducation) {
            setEducation([...education, newEducation]);
        }
    };

    const saveCVData = () => {
        const updatedCV = {
            name,
            about,
            experience,
            skills,
            education,
            salary,
            portfolio,
        };

        if (cvData.id) {
            dispatch(updateCVData({ token, cvData: updatedCV }));
        } else {
            console.log('No need to create CV here, it is handled in fetchCVData');
        }
        dispatch(setShowModal(false));
    };

    const closeModal = () => {
        dispatch(setShowModal(false));
    };

    return (
        showModal && (
            <div className="modal">
                <div className="modal-content">
                    <div className="modal-header">
                        <p>Изменить резюме</p>
                        <svg
                            width="16"
                            height="16"
                            viewBox="0 0 18 18"
                            fill="none"
                            xmlns="http://www.w3.org/2000/svg"
                            onClick={closeModal}
                        >
                            <path
                                d="M1.78125 17.5312L0.46875 16.2188L7.6875 9L0.46875 1.78125L1.78125 0.46875L9 7.6875L16.2188 0.46875L17.5312 1.78125L10.3125 9L17.5312 16.2188L16.2188 17.5312L9 10.3125L1.78125 17.5312Z"
                                fill="black"
                            />
                        </svg>
                    </div>
                    <hr />
                    <div className={style.body}>
                        <div>
                            <label htmlFor="name">Название резюме:</label>
                            <input
                                type="text"
                                id="name"
                                name="name"
                                value={name}
                                onChange={handleInputChange}
                            />
                        </div>
                        <div>
                            <label htmlFor="about">Описание:</label>
                            <textarea
                                id="about"
                                name="about"
                                value={about}
                                onChange={handleInputChange}
                            />
                        </div>
                        <div>
                            <label htmlFor="experience">Опыт работы:</label>
                            <textarea
                                id="experience"
                                name="experience"
                                value={experience}
                                onChange={handleInputChange}
                            />
                        </div>
                        <div>
                            <label>Навыки:</label>
                            <ul>
                                {skills.map((skill, index) => (
                                    <li key={index}>{skill}</li>
                                ))}
                            </ul>
                            <button className={style.skill} onClick={addSkill}>
                                Добавить навык
                            </button>
                        </div>
                        <div>
                            <label>Образование:</label>
                            <ul>
                                {education.map((edu, index) => (
                                    <li key={index}>{edu}</li>
                                ))}
                            </ul>
                            <button className={style.skill} onClick={addEducation}>
                                Добавить образование
                            </button>
                        </div>
                        <div>
                            <label htmlFor="salary">Зарплата:</label>
                            <input
                                type="text"
                                id="salary"
                                name="salary"
                                value={salary}
                                onChange={handleInputChange}
                            />
                        </div>
                        <div>
                            <label htmlFor="portfolio">Портфолио:</label>
                            <input
                                id="portfolio"
                                name="portfolio"
                                value={portfolio}
                                onChange={handleInputChange}
                            />
                        </div>
                        <button className={style.save} onClick={saveCVData}>
                            Сохранить
                        </button>
                    </div>
                </div>
            </div>
        )
    );
};

export default ModalResume;
