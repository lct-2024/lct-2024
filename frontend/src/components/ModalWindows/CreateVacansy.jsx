import React, { useState, useEffect } from 'react';
import style from './CreateVacansy.module.css';
import axios from 'axios';

const CreateVacansy = ({ onClose }) => {
    const token = localStorage.getItem('authToken');
    const [name, setName] = useState('');
    const [about, setAbout] = useState('');
    const [experience, setExperience] = useState('');
    const [salary, setSalary] = useState('');
    const [skills, setSkills] = useState([]);
    const [deadline, setDeadline] = useState('');
    const [category, setCategory] = useState('');
    const [speciality_id, setSpecialityId] = useState(2);

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
            case 'deadline':
                setDeadline(value);
                break;
            case 'skills':
                setDeadline(value);
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

    const handleSubmit = async (e) => {
        e.preventDefault();

        const vacancyData = {
            title: name,
            text: about,
            experience,
            salary,
            skills,
            deadline,
        };

        try {
            const response = await axios.post(
                'https://ats.lct24.dev.40ants.com/api/create_job',
                {
                    jsonrpc: '2.0',
                    method: 'create_job',
                    params: {
                        title: name,
                        description: about,
                        category,
                        speciality_id,
                        salary,
                        project_id: 2
                    },
                    id: 1,
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: token,
                    },
                }
            );

            if (response.ok) {
                const result = await response.data;
                console.log('Vacancy created successfully:', result);
                onClose()
            } else {
                console.error('Failed to create vacancy:', response.statusText);
                onClose()
            }
        } catch (error) {
            console.error('Error creating vacancy:', error);
            onClose()
        }
    };


    return (
        <div className="modal">
            <div className="modal-content">
                <div className="modal-header">
                    <p>Создать вакансию</p>
                    <svg
                        width="16"
                        height="16"
                        viewBox="0 0 18 18"
                        fill="none"
                        xmlns="http://www.w3.org/2000/svg"
                        onClick={onClose}
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
                        <label htmlFor="name">Название:</label>
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
                        <label htmlFor="deadline">Дедлайн:</label>
                        <input
                            type="text"
                            id="deadline"
                            name="deadline"
                            value={deadline}
                            onChange={handleInputChange}
                        />
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
                    <button className={style.save} onClick={handleSubmit} type="submit">
                        Создать
                    </button>
                </div>
            </div>
        </div >
    )
};

export default CreateVacansy;