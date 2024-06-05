import axios from 'axios'
import React, { useState } from 'react'

const VacancyPage = () => {

    const [id, setId] = useState(null)
    const [error, setError] = useState(null)

    const handleChat = async (e) => {
        e.preventDefault()
        setError(null)

        try {
            const response = await axios.post('https://chat.lct24.dev.40ants.com/api/get_chat', {
                jsonrpc: '2.0',
                method: 'get_chat',
                params: {
                    id
                },
            })

            console.log('Response:', response.data)
        } catch (error) {
            setError('Произошла ошибка. Попробуйте позже.');
            console.error('Error:', error)
        }
    }

    const archiveChat = async (e) => {
        e.preventDefault()
        setError(null)

        try {
            const response = await axios.post('https://chat.lct24.dev.40ants.com/api/archive_chat', {
                jsonrpc: '2.0',
                method: 'archive_chat',
                params: {
                    id
                },
            })

            console.log('Response:', response.data)
        } catch (error) {
            setError('Произошла ошибка. Попробуйте позже.');
            console.error('Error:', error)
        }
    }

    return (
        <div>
            <button onClick={handleChat}>Получить чат</button>
            <button onClick={archiveChat}>Архивировать чат</button>
        </div>
    )
}

export default VacancyPage