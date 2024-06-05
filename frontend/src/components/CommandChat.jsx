import React, { useState } from 'react'
import axios from 'axios'

const CommandChat = () => {

    const [content_id, setContentId] = useState(null)
    const [content_type, setContentType] = useState(null)
    const [title, setTitle] = useState("")
    const [isPrivate, setIsPrivate] = useState(false)
    const [error, setError] = useState(null);
    const [succes, setSucces] = useState(false)

    const handleSubmit = async (e) => {
        e.preventDefault()
        setError(null)

        try {
            const chatId = `${content_type}-${content_id}`
            const response = await axios.post('https://chat.lct24.dev.40ants.com/api/create_chat', {
                jsonrpc: '2.0',
                method: 'create_chat',
                params: {
                    content_id,
                    content_type,
                    title,
                    private: isPrivate
                },
                id: chatId
            })

            console.log('Response:', response.data)
            setSucces(true)
            setTimeout(() => {
                setSucces(false)
            }, 2000);
        } catch (error) {
            setError('Произошла ошибка. Попробуйте позже.');
            console.error('Error:', error)
        }
    }

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
            <button type="submit">Зарегистрироваться</button>
            {succes && <div className='alarm'><p>Чат успешно создан</p></div>}
        </form>
    )
}

export default CommandChat