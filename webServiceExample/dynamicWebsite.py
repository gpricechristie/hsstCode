from flask import Flask, render_template, request, redirect, g
import sqlite3

conn = sqlite3.connect("db/test.db")
cursor = conn.cursor()
cursor.execute("DROP TABLE IF EXISTS computing_data;")
cursor.execute("CREATE TABLE IF NOT EXISTS computing_data ( fname TEXT, lname TEXT, valueofcomputing TEXT );")

app = Flask(__name__)

@app.before_request
def before_request():
    g.db = sqlite3.connect("db/test.db")

@app.teardown_request
def teardown_request(exception):
    if hasattr(g, 'db'):
        g.db.close()

@app.route('/add_data', methods = ['POST'])
def add_data():
    fname = request.form['firstname']
    lname = request.form['lastname']
    valueof = request.form['valueofcomputing']

    g.db.execute("INSERT INTO computing_data(fname, lname, valueofcomputing) VALUES (?,?,?)", [fname,lname,valueof])
    g.db.commit()

    return redirect('/')

@app.route('/')
def write_dynamic_page():
    data=g.db.execute("SELECT * FROM computing_data").fetchall()
    displayComments=[]
    for entry in data:
        displayComments.append(entry[0]+" "+entry[1]+" thinks that clinical computing is "+entry[2])

    return render_template('webform.html',len = len(displayComments),displayComments=displayComments)

if __name__ == "__main__":
    app.run()