import React, { useState, useEffect } from "react"
import axios from "axios"

const UserProfile = () => {
    const [userProfile, setUserProfile] = useState(null)

    useEffect(() => {
        const fetchUserProfile = async () => {
            try {
                const token = document.cookie.replace(
                    /(?:(?:^|.*;s*)tokens*=s*([^;]*).*$)|^.*$/,
                    "$1"
                )

                if (!token) {
                    // Если токен отсутствует, просто выходим из функции
                    return
                }
                const headers = {
                    "Authorization": `Bearer ${token}`
                }

                const response = await axios.get(
                    `https://passport.lct24.dev.40ants.com/api/my_profile`,
                    {
                        jsonrpc: "2.0",
                        method: "my_profile",
                        params: {},
                        id: 0
                    },
                    {
                        headers: headers
                    }
                )
                console.log(response)
                setUserProfile(response.data.result)
            } catch (error) {
                console.error("Error:", error)
            }
        }

        fetchUserProfile()
    }, [])

    if (!userProfile) {
        return <div>Loading...</div>
    }

    return (
        <div>
            <h2>Профиль пользователя</h2>
            <p>ФИО: {userProfile.fio}</p>
        </div>
    )
}

export default UserProfile