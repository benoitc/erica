function map(doc) {
    if (doc.type) {
        emit(doc.type, null);
    }
}