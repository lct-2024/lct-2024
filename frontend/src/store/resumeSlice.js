import { createSlice, createAsyncThunk } from '@reduxjs/toolkit';
import axios from 'axios';

// Thunks for async actions
export const fetchCVData = createAsyncThunk(
    'resume/fetchCVData',
    async (token, thunkAPI) => {
        try {
            const response = await axios.post(
                'https://ats.lct24.dev.40ants.com/api/my_cv',
                {
                    jsonrpc: '2.0',
                    method: 'my_cv',
                    params: [],
                    id: 1,
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `${token}`,
                    },
                }
            );

            const data = response.data;

            if (data.error) {
                throw new Error(data.error.message || 'Failed to fetch CV');
            }

            return data.result; // Assuming data.result is an object containing CV details
        } catch (error) {
            throw new Error(error.response?.data?.error?.message || 'Failed to fetch CV');
        }
    }
);

export const createCVData = createAsyncThunk(
    'resume/createCVData',
    async ({ token, cvData }, thunkAPI) => {
        try {
            const response = await axios.post(
                'https://ats.lct24.dev.40ants.com/api/create_cv',
                {
                    jsonrpc: '2.0',
                    method: 'create_cv',
                    params: cvData,
                    id: 1,
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `${token}`,
                    },
                }
            );

            return response.data.result;
        } catch (error) {
            throw new Error(error.response?.data?.error?.message || 'Failed to create CV');
        }
    }
);

export const getCVEducation = createAsyncThunk(
    'resume/getCVEducation',
    async (token, thunkAPI) => {
        try {
            const response = await axios.post(
                'https://ats.lct24.dev.40ants.com/api/get_cv_education',
                {
                    jsonrpc: '2.0',
                    method: 'get_cv_education',
                    params: [],
                    id: 1,
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `${token}`,
                    },
                }
            );

            console.log(response.data)
            const data = response.data;

            if (data.error) {
                throw new Error(data.error.message || 'Failed to fetch education records');
            }

            return data.result; // Assuming data.result is an array of education records
        } catch (error) {
            throw new Error(
                error.response?.data?.error?.message || 'Failed to fetch education records'
            );
        }
    }
);

export const addCVEducation = createAsyncThunk(
    'resume/addCVEducation',
    async ({ token, educationData }, thunkAPI) => {
        try {
            const response = await axios.post(
                'https://ats.lct24.dev.40ants.com/api/add_cv_education',
                {
                    jsonrpc: '2.0',
                    method: 'add_cv_education',
                    params: educationData,
                    id: 1,
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `${token}`,
                    },
                }
            );

            console.log(response.data)
            const data = response.data;

            if (data.error) {
                throw new Error(data.error.message || 'Failed to add education record');
            }

            return data.result; // Assuming data.result is the newly added education record
        } catch (error) {
            throw new Error(
                error.response?.data?.error?.message || 'Failed to add education record'
            );
        }
    }
);

export const deleteCVEducation = createAsyncThunk(
    'resume/deleteCVEducation',
    async ({ token, educationId }, thunkAPI) => {
        try {
            const response = await axios.post(
                'https://ats.lct24.dev.40ants.com/api/delete_cv_education',
                {
                    jsonrpc: '2.0',
                    method: 'delete_cv_education',
                    params: { education_id: educationId },
                    id: 1,
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `${token}`,
                    },
                }
            );
            console.log(response.data)
            const data = response.data;

            if (data.error) {
                throw new Error(data.error.message || 'Failed to delete education record');
            }

            return educationId;
        } catch (error) {
            throw new Error(
                error.response?.data?.error?.message || 'Failed to delete education record'
            );
        }
    }
);

export const updateCVData = createAsyncThunk(
    'resume/updateCVData',
    async ({ token, cvData }, thunkAPI) => {
        try {
            const response = await axios.post(
                'https://ats.lct24.dev.40ants.com/api/update_cv',
                {
                    jsonrpc: '2.0',
                    method: 'update_cv',
                    params: cvData,
                    id: 1,
                },
                {
                    headers: {
                        'Content-Type': 'application/json',
                        Authorization: `${token}`,
                    },
                }
            );

            console.log(response.data.result)
            return response.data.result;
        } catch (error) {
            throw new Error(error.response?.data?.error?.message || 'Failed to update CV');
        }
    }
);

const resumeSlice = createSlice({
    name: 'resume',
    initialState: {
        cvData: {
            about: '',
            chat_id: '',
            contacts: null,
            created_at: '',
            email: '',
            experience: '',
            id: null,
            name: '',
            system_chat_id: '',
            updated_at: '',
            user_id: null,
            skills: [],
            education: [],
        },
        status: 'idle',
        error: null,
        showModal: false,
    },
    reducers: {
        setShowModal: (state, action) => {
            state.showModal = action.payload;
        },
    },
    extraReducers: (builder) => {
        builder
            .addCase(fetchCVData.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(fetchCVData.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.cvData = action.payload;
            })
            .addCase(fetchCVData.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            })
            .addCase(getCVEducation.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(getCVEducation.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.cvData.education = action.payload; // Check action.payload here
                console.log(action.payload); // Debugging output
            })
            .addCase(getCVEducation.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            })
            .addCase(addCVEducation.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(addCVEducation.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.cvData.education.push(action.payload); // Add newly added education record to the array
            })
            .addCase(addCVEducation.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            })
            .addCase(deleteCVEducation.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(deleteCVEducation.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.cvData.education = state.cvData.education.filter(
                    (edu) => edu.id !== action.payload
                );
            })
            .addCase(deleteCVEducation.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            })
            .addCase(createCVData.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(createCVData.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.cvData = action.payload;
                state.showModal = false;
            })
            .addCase(createCVData.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            })
            .addCase(updateCVData.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(updateCVData.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.cvData = action.payload;
                state.showModal = false;
            })
            .addCase(updateCVData.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.error.message;
            });
    },
});

export const { setShowModal } = resumeSlice.actions;

export default resumeSlice.reducer;
