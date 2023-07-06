

# Junior <br> <sup> A fast and simple language for the web. <sup>


## Hello World

Start a new project by running 

```shell
$ mkdir helloWorld
$ cd helloWorld
$ jr new
```

Your folder will now look like 

```
hello_world
│─── package.jron   # configuration
│─── main.jr        # code
│─── .jr            # don't mind this
```

If you look into `package.json` you'll see

```json
{
    "name": "helloWorld",
    "version": "0.0.0",
    "rustEdition": 2018,
    "bins": {
        "default": "main.rs"
    },
    "libs": {
        "default": "main.rs"
    },
    "dependencies": {
        "example": "github.com/example/Example"
    }
}
```

And in `main.jr`

```rs
use std.io

fn main
    io.print "Hello World 👋"
```

To run or only build the project 
```shell 
$ jr run
Hello World 👋
$ jr build
Building Rust...
Building binary...
Finished compiling in 0.1 seconds! ⏰
```

#
## A simple webserver

Creating a web api is very simple. Let's look at `main.rs`.

```rust
use std.{ http, jwt, time, io }
use mydb

let secret = "ncd%jo91XD/!!"

struct AppData {
    db: mydb.Connection
}

fn main
    let server = http.Server.new ()
        .data fn AppData { 
            db = fn mydb.connect "mydburl.com/78427832742somecredential" 
        }
        .route {
            path = "helloWorld"
            action = helloWorld
            middleware = [ authenticate ]
        }
        .route {
            path = "login"
            action = login
        }
        .run 8080

fn helloWorld { name: String } -> String
    "Hello " name "!🍻"

fn login {
    data: AppData
    request: { userName: String, password: String }
} -> http.Response

    let truePassword = db.collection "Users" 
        .where { "userName": userName }
        .get 1
        .first ()

    if truePassword != password
        return (401, "You fucked up the wordssssss")

    let token = jwt.encode {
        data = {
            expiresAt = time.now () + time.minutes 30 
        }
        secret
    }

    token

fn authenticate handle: http.Handler -> http.Handler 
    fn req match req.data.get "token" 
        Some token 
            let { expiresAt } = jwt.decode { secret }
            if time.now () < expiresAt
                handle(req)
            else
                {
                    status = 401
                    message = "Token valid not is anymore"
                }

        None -> {
            status = 404
            message = "I wanna see a token"
        }
```

#
## Or a frontend

```rust
use jr.ui.{ html, browser }

fn main
    browser.show (app, { imagePath = "./images" })

fn app
    ui.widget {
        name = "app"
        state = { imagePath = "" }
        onMessage = fn msg state
            match msg
                ChangeImage path -> state.imagePath = path
        draw = fn state [
            html.text "Yooo 🍻"
            html.input {
                type_ = "text"
                onChange = ChangeImage
            }
            html.image state.imagePath
        ]
        style = {
            display = "flex"
            flexDirection = "column"
            alignItems = "center"
        }
    }
```




