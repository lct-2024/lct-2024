import React, { useState, useEffect } from 'react';
import axios from 'axios';


const config = {
    "servers": [
        {
            "name": "default",
            "url": "https://passport.lct24.dev.40ants.com/"
        }
    ]
};

const UserProfile = () => {
    const [userProfile, setUserProfile] = useState(null);

    useEffect(() => {
        const fetchUserProfile = async () => {
            try {
                // Получите URL из конфигурации
                const serverUrl = config.servers[0].url;
                const token = document.cookie.replace(/(?:(?:^|.*;\s*)token\s*=\s*([^;]*).*$)|^.*$/, '$1');
                const response = await axios.post(`${serverUrl}api/my_profile`, {
                    jsonrpc: '2.0',
                    method: 'my_profile',
                    params: {},
                    id: 0,
                }, {
                    headers: {
                        'Authorization': `Bearer ${token}`,
                    },
                });

                setUserProfile(response.data.result);
            } catch (error) {
                console.error('Error:', error);
            }
        };

        fetchUserProfile();
    }, []);

    if (!userProfile) {
        return <div>Loading...</div>;
    }

    return (
        <div>
            <h2>Профиль пользователя</h2>
            <p>ФИО: {userProfile.fio}</p>
        </div>
    );
};

export default UserProfile;