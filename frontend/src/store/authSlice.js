import { createSlice } from '@reduxjs/toolkit';

const initialState = {
    token: localStorage.getItem('authToken') || null,
    user: JSON.parse(localStorage.getItem('user')) || null,
};

const authSlice = createSlice({
    name: 'auth',
    initialState,
    reducers: {
        setAuthToken(state, action) {
            state.token = action.payload;
            localStorage.setItem('authToken', action.payload);
        },
        setUser(state, action) {
            state.user = action.payload;
            localStorage.setItem('user', JSON.stringify(action.payload));
        },
        logout(state) {
            state.token = null;
            state.user = null;
            localStorage.removeItem('authToken');
            localStorage.removeItem('user');
        }
    }
});

export const { setAuthToken, setUser, logout } = authSlice.actions;

export default authSlice.reducer;
