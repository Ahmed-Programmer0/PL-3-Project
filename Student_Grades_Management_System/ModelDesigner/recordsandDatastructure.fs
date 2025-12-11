module recordsandDatastructure
//  Course Record With Labels And Thir Types
type Course = {
                CourseCode:string;
                CourseName:string;
                CourseGrade:int;
                CourseAppreciation:char
}


// Admin Record With Labels and Their Types
type Admin = {
                AdminName : string;
                NationalId: int;
                AdminEmail: string;
                AdminPassword: string
}



// The Discriminted Union Of AppreciationWord
type WordOfAppreciation = 
                        | A of excellent : string
                        | B of veryGood  : string
                        | C of Good      : string
                        | D of acceptable: string
                        | F of failed    : string


// Student Record With Labels And Their Types
// Note : Courses : list<Course> means That Courses holds a List of Course Objects defined Above 
type Student = {
                Id : int; 
                Name : string; 
                NationalId : int;
                Email : string;
                Password : string;
                yearInCollege : int;
                Courses : list<Course>;
                Gpa : float;
                TotalGrade : float;
                Appreciation : char;
                AppreciationWord : WordOfAppreciation
}

