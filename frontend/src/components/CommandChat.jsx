import React, { useState } from 'react';
import axios from 'axios';
import VacancyPage from './VacancyPage';

const CommandChat = ({ id }) => {
    const [contentId, setContentId] = useState(null);
    const [contentType, setContentType] = useState(null);
    const [title, setTitle] = useState('');
    const [isPrivate, setIsPrivate] = useState(false);
    const [error, setError] = useState(null);
    const [success, setSuccess] = useState(false); // Corrected variable name
    const [chatId, setChatId] = useState(null);

    const handleSubmit = async (e) => {
        e.preventDefault();
        setError(null);
        setSuccess(false);

        try {
            const response = await axios.post('https://chat.lct24.dev.40ants.com/api/create_chat', {
                jsonrpc: '2.0',
                method: 'create_chat',
                params: {
                    content_id: contentId, // Pass content_id even if it's null
                    content_type: contentType, // Pass content_type even if it's null
                    title,
                    private: isPrivate,
                },
                id: 1, // Assuming 'id' is the command ID for your API
            });

            console.log('Response:', response.data);
            setSuccess(true); // Set success message
            setChatId(response.data.result.id);
            console.log(chatId)
            setTimeout(() => {
                setSuccess(false);
            }, 2000);
        } catch (error) {
            setError('Произошла ошибка. Попробуйте позже.');
            console.error('Error:', error);
        }
    };

    return (
        <form onSubmit={handleSubmit}>
            <div>
                <label htmlFor="title">Название чата:</label>
                <input
                    type="text"
                    id="title"
                    value={title}
                    onChange={(e) => setTitle(e.target.value)}
                    required
                />
            </div>

            <div>
                <label htmlFor="isPrivate">Приватный:</label>
                <input
                    type="checkbox"
                    id="isPrivate"
                    checked={isPrivate}
                    onChange={(e) => setIsPrivate(e.target.checked)}
                />
            </div>

            {error && <div className="error">{error}</div>}
            {success && <div className="alarm"><p>Чат успешно создан</p></div>}
            <button type="submit">Зарегистрироваться</button>
            <VacancyPage id={chatId} />
        </form>
    );
};

export default CommandChat;