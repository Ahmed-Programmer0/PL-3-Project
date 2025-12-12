module StudentCrud

open Models

// Check if student ID already exists
let studentIdExists (state: SystemState) (id: int) : bool =
    state.Students |> List.exists (fun s -> s.Id = id)

// Add a new student with manually specified ID
let addStudent (state: SystemState) (student: Student) : OperationResult<SystemState> =
    // Check if ID already exists
    if studentIdExists state student.Id then
        Failure $"Student ID {student.Id} already exists. Please choose a different ID."
    elif student.Id <= 0 then
        Failure "Student ID must be a positive number."
    else
        let newNextId = max state.NextId (student.Id + 1)
        let newState = 
            { state with 
                Students = student :: state.Students
                NextId = newNextId }
        Success newState

// Get student by ID
let getStudentById (state: SystemState) (id: int) : Student option =
    state.Students |> List.tryFind (fun s -> s.Id = id)

// Get all students
let getAllStudents (state: SystemState) : Student list =
    state.Students

// Update student (allows changing ID)
let updateStudent (state: SystemState) (oldId: int) (updatedStudent: Student) : OperationResult<SystemState> =
    match getStudentById state oldId with
    | Some _ ->
        // Check if new ID is different and already exists
        if updatedStudent.Id <> oldId && studentIdExists state updatedStudent.Id then
            Failure $"Cannot change ID to {updatedStudent.Id} - ID already exists!"
        elif updatedStudent.Id <= 0 then
            Failure "Student ID must be a positive number."
        else
            let updatedStudents = 
                state.Students 
                |> List.map (fun s -> if s.Id = oldId then updatedStudent else s)
            let newNextId = max state.NextId (updatedStudent.Id + 1)
            Success { state with Students = updatedStudents; NextId = newNextId }
    | None ->
        Failure $"Student with ID {oldId} not found"

// Delete student (ID becomes available for reuse)
let deleteStudent (state: SystemState) (id: int) : OperationResult<SystemState> =
    match getStudentById state id with
    | Some _ ->
        let filteredStudents = state.Students |> List.filter (fun s -> s.Id <> id)
        Success { state with Students = filteredStudents }
    | None ->
        Failure $"Student with ID {id} not found"

// Search students by name (partial match)
let searchStudentsByName (state: SystemState) (searchTerm: string) : Student list =
    state.Students
    |> List.filter (fun s -> s.Name.ToLower().Contains(searchTerm.ToLower()))

// Get count of students
let getStudentCount (state: SystemState) : int =
    List.length state.Students

// Get list of all existing student IDs (helpful for admin to see what's taken)
let getAllStudentIds (state: SystemState) : int list =
    state.Students 
    |> List.map (fun s -> s.Id) 
    |> List.sort

// Suggest next available ID (helper function, but not enforced)
let suggestNextId (state: SystemState) : int =
    let existingIds = getAllStudentIds state |> Set.ofList
    let rec findGap currentId =
        if Set.contains currentId existingIds then
            findGap (currentId + 1)
        else
            currentId
    if List.isEmpty state.Students then 1 else findGap 1