import axios from 'axios';
import React, { useState } from 'react';

const VacancyPage = ({ id }) => {
    const [error, setError] = useState(null);
    const [chatData, setChatData] = useState(null);

    const handleChat = async (e) => {
        e.preventDefault();
        setError(null);
        setChatData(null); // Clear previous data

        try {
            const response = await axios.post('https://chat.lct24.dev.40ants.com/api/get_chat', {
                jsonrpc: '2.0',
                method: 'get_chat',
                params: {
                    id,
                },
            });

            setChatData(response.data);
        } catch (error) {
            setError('Произошла ошибка. Попробуйте позже.');
            console.error('Error:', error);
        }
    };

    const archiveChat = async (e) => {
        e.preventDefault();
        setError(null);

        try {
            const response = await axios.post('https://chat.lct24.dev.40ants.com/api/archive_chat', {
                jsonrpc: '2.0',
                method: 'archive_chat',
                params: {
                    id,
                },
            });

            console.log('Response:', response.data);
        } catch (error) {
            setError('Произошла ошибка. Попробуйте позже.');
            console.error('Error:', error);
        }
    };

    return (
        <div>
            <button onClick={handleChat}>Получить чат</button>
            <button onClick={archiveChat}>Архивировать чат</button>

            {chatData && (
                <div>
                    <h2>Информация о чате:</h2>
                    <p>ID: {chatData.id}</p>
                    <p>Название: {chatData.title}</p>
                    <p>Создан: {chatData.created_at}</p>
                    <p>Обновлен: {chatData.updated_at}</p>
                    <p>Приватный: {chatData.private ? 'Да' : 'Нет'}</p>
                    <p>Архивирован: {chatData.archived ? 'Да' : 'Нет'}</p>
                    <p>Тип контента: {chatData.content_type}</p>
                    <p>ID контента: {chatData.content_id}</p>
                </div>
            )}

            {/* Display error message if there is one */}
            {error && <div className="error">{error}</div>}
        </div>
    );
};

export default VacancyPage;