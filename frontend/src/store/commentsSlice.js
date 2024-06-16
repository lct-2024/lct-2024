import { createSlice, createAsyncThunk } from '@reduxjs/toolkit';
import axios from 'axios';

const API_BASE_URL = 'https://chat.lct24.dev.40ants.com/api';

export const fetchChat = createAsyncThunk(
    'comments/fetchChat',
    async ({ chatId }, { getState, rejectWithValue }) => {
        const authToken = getState().auth.token;

        const getChat = async (chat_id) => {
            const response = await axios.post(`${API_BASE_URL}/get_chat`, {
                jsonrpc: '2.0',
                method: 'get_chat',
                params: { id: chat_id },
                id: 1,
            }, {
                headers: { 'Authorization': `${authToken}` }
            });
            return response.data.result;
        };

        const getMessages = async (chat_id) => {
            const response = await axios.post(`${API_BASE_URL}/get_messages`, {
                jsonrpc: '2.0',
                method: 'get_messages',
                params: { chat_id },
                id: 1,
            }, {
                headers: { 'Authorization': `${authToken}` }
            });
            return response.data.result.messages;
        };

        try {
            const chat = await getChat(chatId);
            const messages = await getMessages(chatId);
            return { chatId: chat.id, messages };
        } catch (error) {
            return rejectWithValue(error.message);
        }
    }
);

export const postMessage = createAsyncThunk(
    'comments/postMessage',
    async ({ message }, { getState, rejectWithValue }) => {
        const authToken = getState().auth.token;
        const chatId = getState().comments.chatId;

        try {
            const response = await axios.post(`${API_BASE_URL}/post`, {
                jsonrpc: '2.0',
                method: 'post',
                params: { chat_id: chatId, message },
                id: 1,
            }, {
                headers: { 'Authorization': `${authToken}` }
            });
            return response.data.result;
        } catch (error) {
            return rejectWithValue(error.message);
        }
    }
);

const commentsSlice = createSlice({
    name: 'comments',
    initialState: {
        chatId: null,
        comments: [],
        status: 'idle',
        error: null,
    },
    reducers: {},
    extraReducers: (builder) => {
        builder
            .addCase(fetchChat.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(fetchChat.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.chatId = action.payload.chatId;
                state.comments = action.payload.messages;
            })
            .addCase(fetchChat.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.payload;
            })
            .addCase(postMessage.fulfilled, (state, action) => {
                if (!Array.isArray(state.comments)) {
                    state.comments = [];
                }
                state.comments.push(action.payload);
            })
            .addCase(postMessage.rejected, (state, action) => {
                state.error = action.payload;
            });
    },
});

export default commentsSlice.reducer;
