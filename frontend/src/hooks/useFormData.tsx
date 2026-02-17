import { Dispatch, SetStateAction, useState } from "react";
import {
    ConstrainedFormData,
    FieldData,
    FieldError,
    ServerErrorBody,
    SuccessMessage,
} from "../types";
import { ErrorMessage } from "../constants";
import { isServerError, logNetworkError } from "../networkErrorHandlers";
import { objectKeys } from "../utils";

type Helpers<F> = {
    setFormData: Dispatch<SetStateAction<ConstrainedFormData<F>>>;
    handleInputChange: <K extends keyof F>(
        field: K,
        validateBeforeAccept?: (
            value: ConstrainedFormData<F>[K]["value"],
        ) => FieldError,
    ) => (value: ConstrainedFormData<F>[K]["value"]) => void;
    validateField: <K extends keyof F>(field: K) => () => void;
    handleSubmit: () => Promise<void>;
    setSuccessMessage: (message: SuccessMessage) => void;
};

type Meta = {
    errorOnSubmit: FieldError;
    successMessage: SuccessMessage;
    submitting: boolean;
};

interface Args<F, V> {
    initialFormData: ConstrainedFormData<F>;
    optionalFields: string[];
    missingFieldErrorMessage?: ErrorMessage;
    successMessageText?: string;
    submit: (validatedFormData: ConstrainedFormData<V>) => Promise<any>;
    toErrorMessage: (error: ServerErrorBody) => string;
}

export default function useFormData<F, V extends F>({
    initialFormData,
    optionalFields,
    missingFieldErrorMessage = ErrorMessage.Required,
    successMessageText = "Success",
    submit,
    toErrorMessage,
}: Args<F, V>): [ConstrainedFormData<F>, Meta, Helpers<F>] {
    const [formData, setFormData] =
        useState<ConstrainedFormData<F>>(initialFormData);
    const [errorOnSubmit, setErrorOnSubmit] = useState<FieldError>(null);
    const [successMessage, setSuccessMessage] = useState<SuccessMessage>(null);
    const [submitting, setSubmitting] = useState(false);

    const handleInputChange = <K extends keyof ConstrainedFormData<F>>(
        field: K,
        validateBeforeAccept?: (
            value: ConstrainedFormData<F>[K]["value"],
        ) => FieldError,
    ) => {
        return (value: ConstrainedFormData<F>[K]["value"]) => {
            if (validateBeforeAccept) {
                const error: FieldError = validateBeforeAccept(value);
                setFormData((prevData) => ({
                    ...prevData,
                    [field]: error
                        ? { ...prevData[field], error }
                        : { ...prevData[field], error, value },
                }));
            } else {
                setFormData((prevData) => ({
                    ...prevData,
                    [field]: { ...prevData[field], value },
                }));
            }
        };
    };

    const validateField = <K extends keyof ConstrainedFormData<F>>(
        field: K,
    ) => {
        return () => {
            setFormData((prevData) => {
                const error: FieldError =
                    prevData[field].validate() ||
                    (isFieldMissing(field, prevData, optionalFields) &&
                        missingFieldErrorMessage) ||
                    null;

                return error || prevData[field].error
                    ? {
                          ...prevData,
                          [field]: { ...prevData[field], error },
                      }
                    : prevData;
            });
        };
    };

    const validateForm = (): ConstrainedFormData<V> | ErrorMessage.OnSubmit => {
        const stateUpdates = objectKeys(formData).reduce<(() => void)[]>(
            (updates, field) => {
                const error: FieldError =
                    formData[field].validate() ||
                    (isFieldMissing(field, formData, optionalFields) &&
                        missingFieldErrorMessage) ||
                    null;

                if (error) {
                    updates.push(() =>
                        setFormData((prevData) => ({
                            ...prevData,
                            [field]: { ...prevData[field], error },
                        })),
                    );
                }

                return updates;
            },
            [],
        );

        if (!stateUpdates.length) {
            return formData as ConstrainedFormData<V>;
        } else {
            stateUpdates.forEach((update) => update());
            return ErrorMessage.OnSubmit;
        }
    };

    const handleSubmit = async () => {
        try {
            const validatedResult = validateForm();

            if (validatedResult === ErrorMessage.OnSubmit) {
                setErrorOnSubmit(validatedResult as ErrorMessage.OnSubmit);
            } else {
                setErrorOnSubmit(null);
                setSubmitting(true);

                const response = await submit(validatedResult);

                console.log("Form submitted successfully:", response);
                // Handle the response data as needed

                setSuccessMessage(successMessageText);
            }
        } catch (error) {
            logNetworkError(error);
            if (isServerError(error)) {
                setErrorOnSubmit(toErrorMessage(error));
            } else {
                setErrorOnSubmit(ErrorMessage.Default);
            }
        }
        setSubmitting(false);
    };

    return [
        formData,
        {
            errorOnSubmit,
            successMessage,
            submitting,
        },
        {
            setFormData,
            handleInputChange,
            validateField,
            handleSubmit,
            setSuccessMessage,
        },
    ];
}

export function initializeToDefaults<T>(initialValue: T): FieldData<T> {
    return {
        value: initialValue,
        error: null,
        validate: alwaysValid,
    };
}

function isFieldMissing<F>(
    field: keyof ConstrainedFormData<F>,
    formData: ConstrainedFormData<F>,
    optionalFields: string[],
) {
    const { value } = formData[field];
    const isRequired = optionalFields.every((opF) => opF !== field);
    const isEmpty =
        value === null ||
        value === undefined ||
        value === "" ||
        (Array.isArray(value) && !value.length);
    return isEmpty && isRequired;
}

function alwaysValid() {
    return null;
}
