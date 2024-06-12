import React from 'react';

const UserProfile = ({ user }) => {
    return (
        <div>
            <h2>Привет, {user.name}!</h2>
            <p>Ваш email: {user.email}</p>
        </div>
    );
};

export default UserProfile;