import React, { useState } from 'react';
import axios from 'axios';
import style from "./ApplyForm.module.css";

const ApplyForm = ({ jobId }) => {
    const [formData, setFormData] = useState({
        email: '',
        name: '',
        about: '',
        experience: '',
        contacts: [{ type: 'email', value: '' }],
        authToken: null,
    });
    const [errorMessage, setErrorMessage] = useState(null);
    const [successMessage, setSuccessMessage] = useState(null);

    const handleChange = (event) => {
        const { name, value } = event.target;
        setFormData({ ...formData, [name]: value });
    };

    const handleSubmit = async (event) => {
        event.preventDefault();

        try {
            // Step 1: Login to get auth token
            const loginResponse = await axios.post('https://passport.lct24.dev.40ants.com/api/login', {
                jsonrpc: '2.0',
                method: 'login',
                params: {
                    email: formData.email,
                    password: formData.name, // assuming password is same as name for simplicity
                },
                id: 1
            });

            const authToken = loginResponse.data.result;
            setFormData({ ...formData, authToken });

            // Step 2: Create or Update CV
            try {
                await axios.post('https://ats.lct24.dev.40ants.com/api/create_cv', {
                    jsonrpc: '2.0',
                    method: 'create_cv',
                    params: {
                        contacts: formData.contacts,
                        about: formData.about,
                        experience: formData.experience,
                    },
                    id: 1
                }, {
                    headers: {
                        Authorization: `Bearer ${authToken}`
                    }
                });
            } catch (createError) {
                // CV already exists, so update it
                await axios.post('https://ats.lct24.dev.40ants.com/api/update_cv', {
                    jsonrpc: '2.0',
                    method: 'update_cv',
                    params: {
                        contacts: formData.contacts,
                        about: formData.about,
                        experience: formData.experience,
                    },
                    id: 1
                }, {
                    headers: {
                        Authorization: `Bearer ${authToken}`
                    }
                });
            }

            // Step 3: Apply to the job
            await axios.post('https://ats.lct24.dev.40ants.com/api/apply_to_the_job', {
                jsonrpc: '2.0',
                method: 'apply_to_the_job',
                params: {
                    job_id: jobId
                },
                id: 1
            }, {
                headers: {
                    Authorization: `Bearer ${authToken}`
                }
            });

            setSuccessMessage('Вы успешно откликнулись на вакансию!');
            setErrorMessage(null);

        } catch (error) {
            console.error('Ошибка при отклике на вакансию:', error);
            setErrorMessage('Произошла ошибка при отклике на вакансию.');
            setSuccessMessage(null);
        }
    };

    return (
        <div className={style.applyForm}>
            <h2>Откликнуться на вакансию</h2>
            <form onSubmit={handleSubmit}>
                <input
                    type="email"
                    placeholder='Почта'
                    name="email"
                    value={formData.email}
                    onChange={handleChange}
                    required
                />
                <input
                    type="text"
                    placeholder='Имя'
                    name="name"
                    value={formData.name}
                    onChange={handleChange}
                    required
                />
                <input
                    type="text"
                    placeholder='Общее описание кандидата'
                    name="about"
                    value={formData.about}
                    onChange={handleChange}
                />
                <input
                    type="text"
                    placeholder='Описание опыта работы'
                    name="experience"
                    value={formData.experience}
                    onChange={handleChange}
                />
                <button type="submit" className={style.btn}>Откликнуться</button>
            </form>
            {errorMessage && <p className={style.error}>{errorMessage}</p>}
            {successMessage && <p className={style.success}>{successMessage}</p>}
        </div>
    );
};

export default ApplyForm;
