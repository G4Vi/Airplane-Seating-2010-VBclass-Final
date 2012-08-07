Option Strict On
Option Explicit On
Imports System
Imports System.IO
Imports System.Text
Public Class frmPlane
    'Declare some arrays, namesa is the 2 dimensional array holding passenger names at cordinates. Labelarray is self explaning. Seat contains in the beginning the text Taken or Empty for each seat on the plane
    Dim labelarray(39) As Label
    Dim namesa(3, 9) As String
    Dim seat(39) As String
    'This button closes the form
    Private Sub btnClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClose.Click
        Me.Close()
    End Sub
    Private Sub frmPlane_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Declare all variables for form load, open the text file, the c's are other counters.
        Dim sr As StreamReader = File.OpenText("planelist.txt")
        Dim counter As Integer = 0
        Dim c2 As Integer = 0
        Dim c3 As Integer = 0
        Dim c4 As Integer = 0
        Do Until sr.Peek = -1
            'Add the names from the text file to the two dimensional array, starting with the first column to the second column and so forth.
            namesa(c2, counter) = sr.ReadLine
            'Mark the status for each seat added to the array as taken with seat number
            seat(c4) = ("Taken " & c4 + 1)
            counter += 1
            c4 += 1
            If counter = 10 Then
                c2 += 1
                counter = 0
            End If
        Loop
        'Mark the status for each seat left as empty with seat number as long as there is empty seats
        If seat(39) = "" Then
            Do Until c4 = 40
                seat(c4) = ("Empty " & c4 + 1)
                c4 += 1
            Loop
        End If
        'Add each label to the label array
        labelarray(0) = Label1
        labelarray(1) = Label2
        labelarray(2) = Label3
        labelarray(3) = Label4
        labelarray(4) = Label5
        labelarray(5) = Label6
        labelarray(6) = Label7
        labelarray(7) = Label8
        labelarray(8) = Label9
        labelarray(9) = Label10
        labelarray(10) = Label11
        labelarray(11) = Label12
        labelarray(12) = Label13
        labelarray(13) = Label14
        labelarray(14) = Label15
        labelarray(15) = Label16
        labelarray(16) = Label17
        labelarray(17) = Label18
        labelarray(18) = Label19
        labelarray(19) = Label20
        labelarray(20) = Label21
        labelarray(21) = Label22
        labelarray(22) = Label23
        labelarray(23) = Label24
        labelarray(24) = Label25
        labelarray(25) = Label26
        labelarray(26) = Label27
        labelarray(27) = Label28
        labelarray(28) = Label29
        labelarray(29) = Label30
        labelarray(30) = Label31
        labelarray(31) = Label32
        labelarray(32) = Label33
        labelarray(33) = Label34
        labelarray(34) = Label35
        labelarray(35) = Label36
        labelarray(36) = Label37
        labelarray(37) = Label38
        labelarray(38) = Label39
        labelarray(39) = Label40
        'Make the labels color coded, visible, and disabled
        Dim index As Integer
        Dim labeltext As String
        Do Until c3 = 40
            labelarray(c3).Text = seat(c3)
            labelarray(c3).Enabled = False
            labeltext = labelarray(c3).Text
            index = labeltext.IndexOf("Taken")
            If index > -1 Then
                labelarray(c3).BackColor = Color.Red
            Else
                labelarray(c3).BackColor = Color.Green

            End If
            labelarray(c3).Visible = True
            c3 += 1
        Loop
        'Make all of the label clicks execute the same sub
        For Each lbl In labelarray

            AddHandler lbl.Click, AddressOf Label_Click

        Next
        'Disable buttons until text changed subs are called
        btnAdd.Enabled = False
        btnRemove.Enabled = False
    End Sub
    'Adds people to the plane or the waiting list if the plane is full.
    Private Sub btnAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAdd.Click
        'Declares some variables including some counters
        Dim counter As Integer = 0
        Dim fullcounter As Integer = 0
        Dim labeltext As String
        Dim index As Integer
        'Looks at each of the label's text to determine if the seat is not empty, if there are not and open seat it gets added to the waitinglist
        Do Until counter = 40
            labeltext = labelarray(counter).Text
            index = labeltext.IndexOf("Empty")
            If index = -1 Then
                fullcounter += 1
            End If
            counter += 1
            If fullcounter = 40 Then
                lstWait.Items.Add(txtName.Text)
                txtName.Clear()
                Exit Sub
            End If
        Loop
        counter = 0
        'If there was an open seat on the flight, the empty seats labels are enabled so the person can choose a seat
        MessageBox.Show("Click an open seat")
        Do Until counter = 40
            labeltext = labelarray(counter).Text
            index = labeltext.IndexOf("Empty")
            If index > -1 Then
                labelarray(counter).Enabled = True
            Else

            End If
            counter += 1
        Loop
        'Disable the button again
        btnAdd.Enabled = False
    End Sub

    Sub Label_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'Declare the variables.pxl is the label that caused this sub to be executed. L1, L2 are lengths to get the correct part of the 2 d array
        Dim counter, index, L1, L2 As Integer
        Dim pxl = CType(sender, Label)
        Dim tas As String = pxl.Text
        Dim num, namea As String
        index = tas.IndexOf(" ")
        num = tas.Substring(index, tas.Length - index)
        'Visibly show they selected a seat
        pxl.BackColor = Color.Red
        pxl.Text = "Taken " & num
        'Disable all the labels as the seat ws selected
        Do Until counter = 40
            labelarray(counter).Enabled = False
            counter += 1
        Loop
        namea = txtName.Text
        'Place the persons name in the 2 d array under the correct seat
        If num.Length = 2 Then
            namesa(0, CInt(num) - 1) = namea
        Else
            L1 = CInt(num.Substring(1, 1))
            L2 = CInt(num.Substring(2, 1))
            If L2 = 0 Then
                namesa(L1 - 1, 9) = namea
            Else
                namesa(L1, L2 - 1) = namea
            End If

        End If
        'Clear the name field so they can add another name
        txtName.Clear()

    End Sub

    Private Sub btnRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRemove.Click
        'Declare variables for remove button, waitlist is the array for the waiting list
        Dim searchstring, labeltext As String
        Dim counter, index, c2, allcout, c3, c4, c5 As Integer
        Dim waitlist(100) As String
        c3 = 0
        For Each item As Object In lstWait.Items
            waitlist(c3) = item.ToString()
            c3 += 1
        Next
        counter = 0
        c2 = 0
        'Make each value in array not null.
        Do Until c5 = 40
            If namesa(c2, counter) = "" Then
                namesa(c2, counter) = " "
            End If
            c5 += 1
            counter += 1
            If counter = 10 Then
                c2 += 1
                counter = 0
            End If
        Loop
        c2 = 0
        counter = 0
        allcout = 0
        searchstring = txtName2.Text
        If searchstring <> "" Then
            Do Until allcout = 40
                labeltext = namesa(c2, counter)
                index = labeltext.IndexOf(searchstring)
                'If the person is on the flight
                If index > -1 Then
                    'If There is a waiting list
                    If c3 <> 0 Then
                        namesa(c2, counter) = waitlist(0)
                        labelarray(allcout).Text = "Taken " & (allcout + 1)
                        txtName2.Clear()
                        lstWait.Items.Clear()
                        'If the waiting list had more than one person
                        c4 = 0
                        Do Until c4 = c3 - 1
                            lstWait.Items.Add(waitlist(c4 + 1))
                            c4 += 1
                        Loop
                        Exit Sub
                    Else
                        'Display as empty and put a null value in for that seat
                        namesa(c2, counter) = ""
                        labelarray(allcout).Text = "Empty " & (allcout + 1)
                        labelarray(allcout).BackColor = Color.Green
                        txtName2.Clear()
                        Exit Sub
                    End If
                Else
                End If
                counter += 1
                allcout += 1
                If counter = 10 Then
                    c2 += 1
                    counter = 0
                End If

            Loop
            txtName2.Clear()
            MessageBox.Show("Person not found on flight")
        Else
            MessageBox.Show("Please type the name of the person in box.")
        End If
        btnRemove.Enabled = False
    End Sub

    Private Sub txtName_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtName.TextChanged
        btnAdd.Enabled = True
    End Sub

    Private Sub txtName2_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtName2.TextChanged
        btnRemove.Enabled = True
    End Sub
End Class
