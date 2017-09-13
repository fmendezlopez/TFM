package es.uam.eps.tfm.fmendezlopez.dao;

import es.uam.eps.tfm.fmendezlopez.dto.UserRecipe;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.sql.*;

/**
 * Created by franm on 06/09/2017.
 */
public class DatabaseDAO {

    private static final String DBNAME = "SCRAPING_TFM";
    private static final String USERS_TABLENAME = "visited_users";
    private static final String RECIPES_TABLENAME = "visited_recipes";
    private static final String CLASSNAME = "org.apache.derby.jdbc.EmbeddedDriver";
    private static final String DBURL_CREATE = "jdbc:derby:" + DBNAME + ";create=true";
    private static final String DBURL = "jdbc:derby:" + DBNAME + "";
    private Connection conn;

    private static DatabaseDAO ourInstance = new DatabaseDAO();

    public static DatabaseDAO getInstance() {
        return ourInstance;
    }

    private DatabaseDAO() {
    }

    public void connectAndCreate() throws InstantiationException, IllegalAccessException, ClassNotFoundException, SQLException
    {
        Class.forName(CLASSNAME).newInstance();
        conn = DriverManager.getConnection(DBURL_CREATE);
    }

    public void connect() throws InstantiationException, IllegalAccessException, ClassNotFoundException, SQLException
    {
        Class.forName(CLASSNAME).newInstance();
        conn = DriverManager.getConnection(DBURL);
    }

    public void disconnect() throws SQLException, IOException {
        conn.close();
    }

    public void disconnectAndDrop() throws SQLException, IOException {
        dropDatabase();
        conn.close();
    }

    private void dropDatabase() throws IOException{
        String path = System.getProperty("user.dir") + File.separator + DBNAME;

        File db = new File(path);
        FileUtils.deleteDirectory(db);
    }

    private void createUsersTable() throws SQLException{

        String query_recipes = "CREATE TABLE " + USERS_TABLENAME
                + " ("
                + "id VARCHAR(30) PRIMARY KEY"
                + ")";
        Statement stmt = conn.createStatement();
        stmt.execute(query_recipes);
        stmt.close();
    }

    private void createRecipesTable() throws SQLException{

        String query_recipes = "CREATE TABLE " + RECIPES_TABLENAME
                + " ("
                + "id VARCHAR(30) PRIMARY KEY"
                + ")";
        Statement stmt = conn.createStatement();
        stmt.execute(query_recipes);
        stmt.close();
    }

    public void configure() throws SQLException{
        try{
            createUsersTable();
        } catch(SQLException e){
        }
        try{
            createRecipesTable();
        } catch(SQLException e){

        }
    }

    public void insertUser(String id) throws SQLException{
        String query = "INSERT INTO " + USERS_TABLENAME + " VALUES "
                + " ("
                + "'" + id + "'"
                + ")";
        Statement stmt = conn.createStatement();
        stmt.execute(query);
        stmt.close();
    }

    public void insertRecipe(String id) throws SQLException{
        String query = "INSERT INTO " + RECIPES_TABLENAME + " VALUES "
                + " ("
                + "'" + id + "'"
                + ")";
        Statement stmt = conn.createStatement();
        stmt.execute(query);
        stmt.close();
    }

    public boolean existsUser(String url) throws SQLException{
        String query = "SELECT COUNT(*) as number FROM " + USERS_TABLENAME + " WHERE id = '" + url + "'";
        Statement stmt = conn.createStatement();
        ResultSet result = stmt.executeQuery(query);
        result.next();
        int number = result.getInt(1);
        result.close();
        stmt.close();
        boolean ret = number != 0;
        return ret;
    }

    public boolean existsRecipe(String id) throws SQLException{
        String query = "SELECT COUNT(*) as number FROM " + RECIPES_TABLENAME + " WHERE id = '" + id + "'";
        Statement stmt = conn.createStatement();
        ResultSet result = stmt.executeQuery(query);
        result.next();
        int number = result.getInt(1);
        result.close();
        stmt.close();
        boolean ret = number != 0;
        return ret;
    }

    public int countUsers() throws SQLException{
        String query = "SELECT COUNT(*) as number FROM " + USERS_TABLENAME;
        Statement stmt = conn.createStatement();
        ResultSet result = stmt.executeQuery(query);
        result.next();
        int number = result.getInt(1);
        result.close();
        stmt.close();
        return number;
    }

    public int countRecipes() throws SQLException{
        String query = "SELECT COUNT(*) as number FROM " + RECIPES_TABLENAME;
        Statement stmt = conn.createStatement();
        ResultSet result = stmt.executeQuery(query);
        result.next();
        int number = result.getInt(1);
        result.close();
        stmt.close();
        return number;
    }
}
