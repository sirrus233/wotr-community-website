import { useEffect } from "react";

export default function useConditionalActionEffect(
    condition: boolean,
    action: () => void
) {
    useEffect(() => {
        if (condition) action();
    }, [condition]);
}
