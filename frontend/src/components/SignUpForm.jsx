import React, { useState } from 'react';
import axios from 'axios';

const SignupForm = () => {
    const [email, setEmail] = useState('');
    const [fio, setFio] = useState('');
    const [password, setPassword] = useState('');

    const handleSubmit = async (e) => {
        e.preventDefault();

        try {
            const response = await axios.post('https://passport.lct24.dev.40ants.com/api/signup', {
                jsonrpc: '2.0',
                method: 'signup',
                params: {
                    email,
                    fio,
                    password,
                },
                id: 0,
            });

            console.log('Response:', response.data);
            // Сохраняем токен в куках
            document.cookie = `token=${response.data.result}`;
        } catch (error) {
            console.error('Error:', error);
        }
    };

    return (
        <form onSubmit={handleSubmit}>
            <div>
                <label htmlFor="email">Email:</label>
                <input
                    type="email"
                    id="email"
                    value={email}
                    onChange={(e) => setEmail(e.target.value)}
                    required
                />
            </div>
            <div>
                <label htmlFor="fio">ФИО:</label>
                <input
                    type="text"
                    id="fio"
                    value={fio}
                    onChange={(e) => setFio(e.target.value)}
                    required
                />
            </div>
            <div>
                <label htmlFor="password">Пароль:</label>
                <input
                    type="password"
                    id="password"
                    value={password}
                    onChange={(e) => setPassword(e.target.value)}
                    required
                />
            </div>
            <button type="submit">Зарегистрироваться</button>
        </form>
    );
};

export default SignupForm;