import React, { useState, useEffect } from 'react';
import style from './ModalUser.module.css';

const ModalUser = ({ user, onClose, onSave }) => {
    const [name, setName] = useState('');
    const [phone, setPhone] = useState('');
    const [dateB, setdateB] = useState('');

    const saveCVData = () => {
        const updatedUser = {
            ...user,
            fio: name,
            phone: phone,
            dateB: dateB
        };
        onSave(updatedUser)
    };


    const handleInputChange = (e) => {
        const { name, value } = e.target;
        switch (name) {
            case 'name':
                setName(value);
                break;
            case 'phone':
                setPhone(value);
                break;
            case 'dateB':
                setdateB(value);
                break;
            default:
                break;
        }
    };

    return (
        <div className="modal">
            <div className={style.modalContent}>
                <div className="modal-header">
                    <p>Изменить профиль</p>
                    <svg
                        width="16"
                        height="16"
                        viewBox="0 0 18 18"
                        fill="none"
                        xmlns="http://www.w3.org/2000/svg"
                        onClick={onClose}
                    >
                        <path
                            d="M1.78125 17.5312L0.46875 16.2188L7.6875 9L0.46875 1.78125L1.78125 0.46875L9 7.6875L16.2188 0.46875L17.5312 1.78125L10.3125 9L17.5312 16.2188L16.2188 17.5312L9 10.3125L1.78125 17.5312Z"
                            fill="black"
                        />
                    </svg>
                </div>
                <hr />
                <div className={style.body}>
                    <div>
                        <label htmlFor="name">Фамилия имя отчество:</label>
                        <input
                            type="text"
                            id="name"
                            name="name"
                            value={name}
                            onChange={handleInputChange}
                        />
                    </div>
                    <div>
                        <label htmlFor="phone">Номер телефона:</label>
                        <input
                            type='text'
                            id="phone"
                            name="phone"
                            value={phone}
                            onChange={handleInputChange}
                        />
                    </div>
                    <div>
                        <label htmlFor="dateB">Дата рождения:</label>
                        <input
                            type='text'
                            id="dateB"
                            name="dateB"
                            value={dateB}
                            onChange={handleInputChange}
                        />
                    </div>
                    <button className={style.save} onClick={saveCVData}>
                        Сохранить
                    </button>
                </div>
            </div>
        </div>
    );
};

export default ModalUser;
