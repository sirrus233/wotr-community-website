import { sum } from "../../utils";

export default function sumPriorWidths(
    headers: { width: number }[],
    position: number,
) {
    return sum(headers.slice(0, position).map((h) => h.width));
}
