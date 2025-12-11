namespace StudentManagement

open System
open System.IO
open System.Text.Json

type Course = {
    Code: string
    Name: string
    Score: float
    mutable LetterGrade: string 
    mutable Description: string 
}

type Student = {
    Id: string
    Name: string
    Courses: Course list
}


module GradeCalculator =


    let getGradeDetails (score: float) =
        match score with
        | courseScore when courseScore >= 90.0 -> ("A", "Excellent")
        | courseScore when courseScore >= 85.0 -> ("A-", "Excellent")
        | courseScore when courseScore >= 80.0 -> ("B+", "Very Good")
        | courseScore when courseScore >= 75.0 -> ("B", "Very Good")
        | courseScore when courseScore >= 70.0 -> ("B-", "Good")
        | courseScore when courseScore >= 65.0 -> ("C+", "Good")
        | courseScore when courseScore >= 60.0 -> ("C", "Pass")
        | courseScore when courseScore >= 56.0 -> ("C-", "Pass")
        | courseScore when courseScore >= 53.0 -> ("D+", "Poor")
        | courseScore when courseScore >= 50.0 -> ("D", "Poor")
        | _ -> ("F", "Fail")

    
    let calculateAverage (student: Student) : float =
        match student.Courses with
        | [] -> 0.0
        | courses -> 
            let total = courses |> List.sumBy (fun c -> c.Score)
            total / float courses.Length

    /// Populates the mutable LetterGrade and Description fields.
    let processStudentGrades (student: Student) =
        student.Courses |> List.iter (fun course ->
            let (letter, desc) = getGradeDetails course.Score
            course.LetterGrade <- letter
            course.Description <- desc
        )

    
    let getStudentSummary (student: Student) : string =
        let avg = calculateAverage student
        let (avgLetter, _) = getGradeDetails avg
        sprintf "%-15s | %-10s | Avg: %5.2f (%s)" student.Name student.Id avg avgLetter


module DataLoader =
    
    let loadStudents (filePath: string) : Student list =
        // THIS IS THE LINE THAT IMPORTS DATA FROM THE FILE
        let jsonString = File.ReadAllText(filePath)
        JsonSerializer.Deserialize<Student list>(jsonString)
