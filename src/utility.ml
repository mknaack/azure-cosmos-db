let authorization_token_using_master_key verb resource_type resource_id date master_key =
(* var key = new Buffer(masterKey, "base64");   *)

(*     var text = (verb || "").toLowerCase() + "\n" +    *)
(*                (resourceType || "").toLowerCase() + "\n" +    *)
(*                (resourceId || "") + "\n" +    *)
(*                date.toLowerCase() + "\n" +    *)
(*                "" + "\n";   *)

(*     var body = new Buffer(text, "utf8");   *)
(*     var signature = crypto.createHmac("sha256", key).update(body).digest("base64");   *)

  let  master_token = "master" in
  
  let token_version = "1.0" in
  "type=" ^ master_token ^ "&ver=" ^ token_version ^ "&sig="
(*     return encodeURIComponent("type=" + MasterToken + "&ver=" + TokenVersion + "&sig=" + signature);  *)
