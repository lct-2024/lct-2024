import React, { useState, useEffect } from "react";
import axios from "axios";

const UserProfile = () => {
    const [profile, setProfile] = useState(null);
    const [error, setError] = useState(null);

    useEffect(() => {
        fetchProfile();
    }, []);

    const fetchProfile = async () => {
        try {
            const response = await axios.get(
                "https://passport.lct24.dev.40ants.com/api/my_profile"
            );

            setProfile(response.data);
        } catch (error) {
            setError("Произошла ошибка. Попробуйте позже.");
            console.error("Error:", error);
        }
    };

    if (error) {
        return <div className="error">{error}</div>;
    }

    if (!profile) {
        return <div>Loading...</div>;
    }

    return (
        <div>
            <h2>Мой профиль</h2>
            <p>
                Имя: {profile.fio}<br />
                Email: {profile.email}<br />
                Должность: {profile.position}<br />
                Администратор: {profile.admin ? "Да" : "Нет"}<br />
            </p>
        </div>
    );
};

export default UserProfile;