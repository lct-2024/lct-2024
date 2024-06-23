import React, { useState } from 'react';
import style from "./Login.module.css";
import Footer from '../Footer';
import Navigation from '../Navigation';
import { Link, useNavigate } from 'react-router-dom';
import axios from 'axios';
import { useDispatch } from 'react-redux';
import { setAuthToken, setUser } from '../../store/authSlice';

const Login = () => {
    const [formData, setFormData] = useState({
        email: '',
        password: '',
    });
    const [errorMessage, setErrorMessage] = useState(null);
    const navigate = useNavigate();
    const dispatch = useDispatch();

    const handleChange = (event) => {
        const { name, value } = event.target;
        setFormData({ ...formData, [name]: value });
    };

    const handleSubmit = async (event) => {
        event.preventDefault();

        try {
            const response = await axios.post('https://passport.lct24.dev.40ants.com/api/login', {
                jsonrpc: '2.0',
                method: 'login',
                params: {
                    email: formData.email,
                    password: formData.password,
                },
                id: 1
            });

            console.log('Response:', response);

            if (response.data.error) {
                setErrorMessage(response.data.error.message || 'Ошибка входа');
            } else {
                const result = response.data.result;
                const token = result.token;
                const user = result.user;
                console.log(user)
                console.log('Успешный вход:', token);
                dispatch(setUser(user));
                dispatch(setAuthToken(token));
                localStorage.setItem('authToken', token);
                localStorage.setItem('user', JSON.stringify(user));
                navigate('/profile');
            }
        } catch (error) {
            console.error('Ошибка при отправке данных:', error);
            setErrorMessage('Произошла ошибка при входе');
        }
    };

    return (
        <div className={style.wrap}>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>ВХОД</h1>
                    </div>
                </div>
            </div>
            <div className='container'>
                <form onSubmit={handleSubmit} className={style.body}>
                    <p>Чтобы оставлять резюме, комментировать и общаться с рекрутерами. <Link to="/registration">Нет аккаунта, зарегистрироваться</Link></p>
                    <input
                        type="email"
                        placeholder='Почта'
                        name="email"
                        value={formData.email}
                        onChange={handleChange}
                        required
                    />
                    <input
                        type="password"
                        placeholder='Пароль'
                        name="password"
                        value={formData.password}
                        onChange={handleChange}
                        required
                    />
                    <button type="submit" className={style.btn}>Войти</button>

                    {errorMessage && <p className={style.error}>{errorMessage}</p>}
                </form>
            </div>
            <Footer />
        </div>
    );
};

export default Login;
